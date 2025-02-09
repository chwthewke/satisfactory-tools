package net.chwthewke.satisfactorytools
package web.app

import cats.Apply
import cats.Eq
import cats.Semigroup
import cats.data.EitherT
import cats.data.OptionT
import cats.effect.Async
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.nested._
import cats.syntax.semigroup._
import cats.syntax.semigroupk._
import cats.syntax.show._
import org.http4s.ContextRequest
import org.http4s.ContextRoutes
import org.http4s.FormDataDecoder
import org.http4s.HttpRoutes
import org.http4s.Request
import org.http4s.Response
import org.http4s.Uri
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.Location
import org.http4s.server.ContextMiddleware
import org.http4s.syntax.literals._

import api.LibraryApi
import api.ModelApi
import api.PlannerApi
import api.SessionApi
import data.Item
import model.Model
import prod.Factory
import protocol.InputTab
import protocol.ItemIO
import protocol.ItemSrcDest
import protocol.OutputTab
import protocol.PlanHeader
import protocol.PlanId
import protocol.PlanName
import protocol.Session
import protocol.SolutionHeader
import web.forms
import web.forms.Actions
import web.forms.Decoders
import web.view.CompareView
import web.view.LibraryView
import web.view.PlanView

class Application[F[_]](
    val models: ModelApi[F],
    val library: LibraryApi[F],
    val planner: PlannerApi[F],
    val sessionMiddleware: ContextMiddleware[F, Session]
)( implicit F: Async[F] ) {

  private implicit val dsl: Http4sDsl[F] = new Http4sDsl[F] {}
  import dsl._

  import Application.segment

  private implicit def semigroupForF[A: Semigroup]: Semigroup[F[A]] = Apply.semigroup

  val libraryRoutes: ContextRoutes[Session, F] = ContextRoutes.of[Session, F] {
    case ContextRequest( session, GET -> Root ) =>
      for {
        plans         <- library.getAllPlans( session.userId )
        modelVersions <- models.getModelVersions
        response      <- Ok( LibraryView.viewAllPlans( plans, modelVersions.reverse ) )
      } yield response

    case ContextRequest( session, req @ POST -> Root / "new" ) =>
      for {
        modelVersion <- decode( req )( forms.Decoders.modelVersion )
        planId       <- planner.newPlan( session.userId, modelVersion )
        response     <- redirect( planId, InputTab.Bill, OutputTab.Steps( editGroups = false ) )
      } yield response

    case ContextRequest( session, POST -> Root / "library" ) =>
      Found( Location( uri"/" ) )

    case ContextRequest( session, POST -> Root / "delete" / segment.PlanId( id ) ) =>
      Found( Location( uri"/delete" / id.show / "" ) )

    case ContextRequest( session, GET -> Root / "delete" / segment.PlanId( id ) ) =>
      planner
        .getPlanHeader( id )
        .toRightF( NotFound() )
        .semiflatMap( header => Ok( LibraryView.deleteConfirm( header ) ) )
        .merge

    case ContextRequest( session, POST -> Root / "delete" / segment.PlanId( id ) / "confirm" ) =>
      library.deletePlan( session.userId, id ) *> Found( Location( uri"/" ) )

    case ContextRequest( session, POST -> Root / "delete" / segment.PlanId( id ) / "cancel" ) =>
      Found( Location( uri"/" ) )

    case ContextRequest( session, req @ POST -> Root / "compare" ) =>
      OptionT( decode( req )( forms.Decoders.comparePlans ) )
        .foldF( Found( Location( uri"/" ) ) ) {
          case ( b, a ) => Found( Location( uri"/" / "compare" / show"$b" / show"$a" ) )
        }

  }

  val planRoutes: ContextRoutes[Session, F] = ContextRoutes.of {
    // TODO make view read-only if plan owner != session user
    case ContextRequest( session, GET -> Root / "plan" / segment.PlanId( planId ) ) =>
      redirect( planId, InputTab.Bill, OutputTab.Steps( editGroups = false ) )

    case ContextRequest(
          session,
          GET -> Root / "plan" / segment.PlanId( planId ) /
          segment.InputTab( inputTab ) / segment.OutputTab( outputTab )
        ) =>
      viewPlan( planId, inputTab, outputTab )

    case ContextRequest(
          session,
          req @ POST -> "plan" /: segment.PlanId( planId ) /: segment.InputTab( inputTab ) /:
          segment.OutputTab( outputTab ) /: rest
        ) =>
      updatePlan[inputTab.Data, outputTab.Data]( planId, inputTab, outputTab, req, rest )

    case ContextRequest( session, GET -> Root / "compare" / segment.PlanId( before ) / segment.PlanId( after ) ) =>
      comparePlans( before, after )

  }

  private type CompareFactory = ( Factory, Map[Item, ItemIO[ItemSrcDest.Global]] )

  private def comparePlans( before: PlanId, after: PlanId ): F[Response[F]] = {
    def getFactory( planId: PlanId ): OptionT[F, CompareFactory] =
      planner
        .getPlanHeader( planId )
        .map( header => header.solution )
        .flatMapF( solutionHeader =>
          (
            planner.getPlanResult( planId, solutionHeader, OutputTab.Steps( editGroups = false ) ).nested,
            planner.getPlanResult( planId, solutionHeader, OutputTab.Items ).nested
          ).mapN { case ( ( factory, _ ), items ) => ( factory, items ) }.value.map( _.value )
        )

    val compareOrError: EitherT[F, String, ( CompareFactory, CompareFactory )] = for {
      headerBefore <- planner.getPlanHeader( before ).toRight( "Missing header before" )
      headerAfter  <- planner.getPlanHeader( after ).toRight( "Missing header after" )
      _ <- EitherT.cond( headerBefore.modelVersionId == headerAfter.modelVersionId, (), "differing model versions" )
      factoryBefore <- getFactory( before ).toRight( "Missing solution before" )
      factoryAfter  <- getFactory( after ).toRight( "Missing solution after" )
    } yield ( factoryBefore, factoryAfter )

    Ok(
      compareOrError.fold(
        err => CompareView( err ),
        facs => CompareView( facs._1, facs._2 )
      )
    )

  }

  private def getHeaderAndModel( planId: PlanId ): EitherT[F, Response[F], ( PlanHeader, Model )] =
    planner
      .getPlanHeader( planId )
      .toRightF( NotFound() )
      .mproduct( header => models.getModel( header.modelVersionId ).toRightF( NotFound() ) )

  private def viewPlan( planId: PlanId, inputTab: InputTab, outputTab: OutputTab ): F[Response[F]] =
    viewPlanAux[inputTab.Data, outputTab.Data]( planId, inputTab, outputTab )

  private def viewPlanAux[I, O](
      planId: PlanId,
      inputTab: InputTab.Aux[I],
      outputTab: OutputTab.Aux[O]
  ): F[Response[F]] =
    getHeaderAndModel( planId ).semiflatMap {
      case ( header, model ) =>
        for {
          inputData       <- planner.getPlanQuery( planId, inputTab )
          outputData      <- planner.getPlanResult( planId, header.solution, outputTab )
          migrationTarget <- models.getModelVersions.map( _.lastOption.map( _._2 ) )
          response <- Ok( PlanView( model, migrationTarget, header, inputTab, inputData, outputTab, outputData ) )
        } yield response
    }.merge

  private def updatePlan[I, O](
      planId: PlanId,
      inputTab: InputTab.Aux[I],
      outputTab: OutputTab.Aux[O],
      request: Request[F],
      actionPath: Uri.Path
  ): F[Response[F]] =
    getHeaderAndModel( planId ).semiflatMap {
      case ( header, model ) =>
        val actionChanges: Option[PlanId => F[Unit]] = collectRecipeActionChanges( actionPath )
        for {
          changes  <- collectAllChanges( planId, model, inputTab, outputTab, request, actionPath ).value
          targetId <- targetPlanId( header, request, changes.isDefined, actionPath )
          _        <- ( changes |+| actionChanges ).traverse_( act => act( targetId ) )
          _ <- planner
                 .computePlan( targetId )
                 .whenA( computeAction( changes.isDefined, header.solution, actionPath ) )
          _ <- resetTreeAction( planId, actionPath )
          _ <- treeAction( planId, actionPath, request )
          _ <- customGroupActions( targetId, changes.isDefined, outputTab, actionPath )
          ( nextInputTab, nextOutputTab ) = destination( inputTab, outputTab, actionPath )
          response <- redirect( targetId, nextInputTab, nextOutputTab )
        } yield response
    }.merge

  private def targetPlanId(
      header: PlanHeader,
      request: Request[F],
      hasChanges: Boolean,
      actionPath: Uri.Path
  ): F[PlanId] =
    actionPath match {
      case Actions.save /: _ =>
        decode( request )( Decoders.title )
          .flatMap( nameOpt => library.savePlan( header, nameOpt.getOrElse( PlanName( show"Plan #${header.id}" ) ) ) )
      case Actions.migrate /: _ =>
        library.migratePlan( header.owner, header.id )
      case _ =>
        library.editPlan( header, hasChanges )
    }

  private def computeAction[X]( hasChanges: Boolean, solution: SolutionHeader[X], actionPath: Uri.Path ): Boolean =
    actionPath match {
      case Actions.compute /: _ => true
      case _                    => hasChanges && solution.isComputed
    }

  private def resetTreeAction( planId: PlanId, path: Uri.Path ): F[Unit] =
    path match {
      case Actions.tree.resetPath => planner.resetTreeCommands( planId )
      case _                      => F.unit
    }

  private def treeAction( planId: PlanId, path: Uri.Path, request: Request[F] ): F[Unit] =
    Actions.tree
      .command( path, request )
      .fold( F.unit )(
        _.flatMap( cmd => planner.recordTreeCommand( planId, cmd ) )
      )

  private def customGroupActions[O](
      targetId: PlanId,
      hasChanges: Boolean,
      outputTab: OutputTab.Aux[O],
      actionPath: Uri.Path
  ): F[Unit] =
    planner.addCustomGroup( targetId ).whenA( addCustomGroup( actionPath ) ) *>
      planner.removeCustomGroup( targetId ).whenA( removeCustomGroup( actionPath ) ) *>
      reorderGroup( targetId, outputTab, actionPath ).whenA( !hasChanges )

  private def addCustomGroup( actionPath: Uri.Path ): Boolean =
    actionPath match {
      case Actions.addGroup /: _ => true
      case _                     => false
    }

  private def removeCustomGroup( actionPath: Uri.Path ): Boolean =
    actionPath match {
      case Actions.removeGroup /: _ => true
      case _                        => false
    }

  private def reorderGroup( planId: PlanId, outputTab: OutputTab, actionPath: Uri.Path ): F[Unit] =
    ( outputTab, actionPath ) match {
      case ( OutputTab.CustomGroup( ix, _ ), Actions.outputGroupMoveDown /: segment.Int( row ) /: _ ) =>
        planner.swapCustomGroupRowWithNext( planId, ix, row )
      case ( OutputTab.CustomGroup( ix, _ ), Actions.outputGroupMoveUp /: segment.Int( row ) /: _ ) =>
        planner.swapCustomGroupRowWithPrevious( planId, ix, row )
      case ( OutputTab.CustomGroup( ix, _ ), Actions.outputGroupSection /: segment.Int( row ) /: _ ) =>
        planner.toggleCustomGroupSectionBefore( planId, ix, row )
      case _ =>
        F.unit
    }

  private def destination( inputTab: InputTab, outputTab: OutputTab, actionPath: Uri.Path ): ( InputTab, OutputTab ) =
    actionPath match {
      case "input" /: segment.InputTab( newInputTab ) /: _    => ( newInputTab, outputTab )
      case "output" /: segment.OutputTab( newOutputTab ) /: _ => ( inputTab, newOutputTab )
      case _                                                  => ( inputTab, outputTab )
    }

  private def collectAllChanges(
      planId: PlanId,
      model: Model,
      inputTab: InputTab,
      outputTab: OutputTab,
      request: Request[F],
      actionPath: Uri.Path
  ): OptionT[F, PlanId => F[Unit]] =
    collectInputChanges( planId, model, inputTab, request ) |+|
      collectOutputChanges( planId, outputTab, request )

  private def collectInputChanges(
      planId: PlanId,
      model: Model,
      inputTab: InputTab,
      request: Request[F]
  ): OptionT[F, PlanId => F[Unit]] = inputTab match {
    case InputTab.Bill =>
      collectChanges(
        request,
        Decoders.bill( model ),
        planner.getPlanQuery( planId, InputTab.Bill ),
        planner.setBill
      )
    case InputTab.Recipes =>
      collectChanges(
        request,
        Decoders.recipeList( model ),
        planner.getPlanQuery( planId, InputTab.Recipes ),
        planner.setRecipeList
      )
    case InputTab.Options =>
      collectChanges(
        request,
        Decoders.options,
        planner.getPlanQuery( planId, InputTab.Options ),
        planner.setOptions
      )
    case InputTab.ResourceOptions =>
      collectChanges(
        request,
        Decoders.resourceOptions( model ),
        planner.getPlanQuery( planId, InputTab.ResourceOptions ),
        planner.setResourceOptions
      )
  }

  private def collectOutputChanges(
      planId: PlanId,
      outputTab: OutputTab,
      request: Request[F]
  ): OptionT[F, PlanId => F[Unit]] =
    outputTab match {
      case OutputTab.Steps( true ) =>
        collectChanges(
          request,
          Decoders.customGroups,
          planner.getCustomGroupSelection( planId ),
          planner.setCustomGroupSelection
        )
      case _ => OptionT.none
    }

  private def collectChanges[A: Eq](
      request: Request[F],
      decoder: FormDataDecoder[A],
      readValue: F[A],
      writeValue: ( PlanId, A ) => F[Unit]
  ): OptionT[F, PlanId => F[Unit]] =
    OptionT
      .liftF( ( decode( request )( decoder ), readValue ).tupled )
      .flatMap {
        case ( newValue, oldValue ) =>
          OptionT.when( newValue =!= oldValue )( writeValue( _, newValue ) )
      }

  private def collectRecipeActionChanges( actionPath: Uri.Path ): Option[PlanId => F[Unit]] =
    actionPath match {
      case Actions.addAllRecipes /: _ =>
        Some( planner.addAllRecipes )
      case Actions.addAlts /: _ =>
        Some( planner.addAllAlternatesToRecipeList )
      case Actions.removeAlts /: _ =>
        Some( planner.removeAllAlternatesFromRecipeList )
      case Actions.removeMatterConversion /: _ =>
        Some( planner.removeMatterConversionFromRecipeList )
      case Actions.lockRecipes /: _ =>
        Some( planner.lockCurrentRecipes )
      case Actions.recipesUpToTier( tier, withAlts ) /: _ =>
        Some( planner.addRecipesUpToTier( _, tier, withAlts ) )
      case _ => None
    }

  private def decode[A]( request: Request[F] )( implicit formDataDecoder: FormDataDecoder[A] ): F[A] = {
    import FormDataDecoder.formEntityDecoder
    request.as[A]
  }

  private def redirect(
      planId: PlanId,
      inputTab: InputTab,
      outputTab: OutputTab
  ): F[Response[F]] =
    Found( Location( uri"/plan" / planId.show / Actions.input( inputTab ) / Actions.output( outputTab ) / "" ) )

  val routes: HttpRoutes[F] = sessionMiddleware( libraryRoutes <+> planRoutes )

}

object Application {

  def apply[F[_]: Async](
      modelApi: ModelApi[F],
      sessionApi: SessionApi[F],
      libraryApi: LibraryApi[F],
      plannerApi: PlannerApi[F]
  ): Application[F] =
    new Application[F]( modelApi, libraryApi, plannerApi, SessionMiddleware[F]( sessionApi ) )

  val cookieSessionIdKey = "session-id"

  object segment {
    trait Segment[A] { self =>
      def unapply( segment: String ): Option[A]

      private[segment] def omap[B]( f: A => Option[B] ): Segment[B] =
        ( segment: String ) => self.unapply( segment ).flatMap( f )

      private[segment] def map[B]( f: A => B ): Segment[B]                = omap( a => Some( f( a ) ) )
      private[segment] def emap[E, B]( f: A => Either[E, B] ): Segment[B] = omap( a => f( a ).toOption )

    }

    val String: Segment[String]       = Some( _ )
    val Int: Segment[Int]             = String.omap( Numeric[Int].parseString )
    val PlanId: Segment[PlanId]       = Int.map( protocol.PlanId( _ ) )
    val InputTab: Segment[InputTab]   = String.omap( Actions.input.unapply )
    val OutputTab: Segment[OutputTab] = String.omap( Actions.output.unapply )

  }
}
