package net.chwthewke.satisfactorytools
package web.app

import cats.Apply
import cats.Eq
import cats.Semigroup
import cats.data.OptionT
import cats.effect.Async
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.semigroup._
import cats.syntax.semigroupk._
import cats.syntax.show._
import cats.syntax.traverse._
import org.http4s.ContextRequest
import org.http4s.ContextRoutes
import org.http4s.FormDataDecoder
import org.http4s.HttpRoutes
import org.http4s.Request
import org.http4s.Response
import org.http4s.Uri
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.Location
import org.http4s.scalatags._
import org.http4s.server.ContextMiddleware
import org.http4s.syntax.literals._

import api.LibraryApi
import api.PlannerApi
import api.SessionApi
import model.Model
import model.Options
import protocol.InputTab
import protocol.OutputTab
import protocol.PlanHeader
import protocol.PlanId
import protocol.PlanName
import protocol.Session
import protocol.SolutionHeader
import web.forms.Actions
import web.forms.Decoders
import web.view.LibraryView
import web.view.PlanView

class Application[F[_]](
    val model: Model,
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
      library.getAllPlans( session.userId ).flatMap( plans => Ok( LibraryView.viewAllPlans( plans ) ) )

    case ContextRequest( session, POST -> Root / "new" ) =>
      planner
        .newPlan( session.userId, Options.default, model.defaultResourceOptions )
        .flatMap( redirect( _, InputTab.Bill, OutputTab.Steps ) )

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
  }

  val planRoutes: ContextRoutes[Session, F] = ContextRoutes.of {
    // TODO make view read-only if plan owner != session user
    case ContextRequest( session, GET -> Root / "plan" / segment.PlanId( planId ) ) =>
      redirect( planId, InputTab.Bill, OutputTab.Steps )

    case ContextRequest(
        session,
        GET -> Root / "plan" / segment.PlanId( planId ) / segment.InputTab( inputTab ) / segment.OutputTab( outputTab )
        ) =>
      viewPlan( planId, inputTab, outputTab )

    case ContextRequest(
        session,
        req @ POST -> "plan" /: segment.PlanId( planId ) /: segment.InputTab( inputTab ) /:
          segment.OutputTab( outputTab ) /: rest
        ) =>
      updatePlan[inputTab.Data, outputTab.Data]( planId, inputTab, outputTab, req, rest )

  }

  private def viewPlan( planId: PlanId, inputTab: InputTab, outputTab: OutputTab ): F[Response[F]] =
    viewPlanAux[inputTab.Data, outputTab.Data]( planId, inputTab, outputTab )

  private def viewPlanAux[I, O](
      planId: PlanId,
      inputTab: InputTab.Aux[I],
      outputTab: OutputTab.Aux[O]
  ): F[Response[F]] = {
    planner
      .getPlanHeader( planId )
      .toRightF( NotFound() )
      .semiflatMap(
        header =>
          for {
            inputData  <- planner.getPlanQuery( planId, inputTab )
            outputData <- header.solution.traverse( planner.getPlanResult( planId, _, outputTab ) )
            response   <- Ok( PlanView( model, header, inputTab, inputData, outputTab, outputData ) )
          } yield response
      )
      .merge
  }

  private def updatePlan[I, O](
      planId: PlanId,
      inputTab: InputTab.Aux[I],
      outputTab: OutputTab.Aux[O],
      request: Request[F],
      actionPath: Uri.Path
  ): F[Response[F]] =
    planner
      .getPlanHeader( planId )
      .toRightF( NotFound() )
      .semiflatMap(
        header =>
          for {
            changes  <- collectAllChanges( planId, inputTab, outputTab, request ).value
            targetId <- targetPlanId( header, request, changes.isDefined, actionPath )
            _        <- changes.traverse_( act => act( targetId ) )
            _ <- planner
                  .computePlan( targetId )
                  .whenA( computeAction( changes.isDefined, header.solution, actionPath ) )
            _ <- planner
                  .addCustomGroup( targetId )
                  .whenA( addCustomGroup( actionPath ) )
            _ <- planner
                  .removeCustomGroup( targetId )
                  .whenA( removeCustomGroup( actionPath ) )
            _ <- reorderGroup( targetId, changes.isDefined, outputTab, actionPath )
            ( nextInputTab, nextOutputTab ) = destination( inputTab, outputTab, actionPath )
            response <- redirect( targetId, nextInputTab, nextOutputTab )
          } yield response
      )
      .merge

  private def targetPlanId(
      header: PlanHeader,
      request: Request[F],
      hasChanges: Boolean,
      actionPath: Uri.Path
  ): F[PlanId] =
    actionPath match {
      case "save" /: _ =>
        decode( request )( Decoders.title )
          .flatMap(
            nameOpt =>
              library.savePlan(
                header.owner,
                header.id,
                header.copy,
                nameOpt.getOrElse( PlanName( show"Plan #${header.id}" ) )
              )
          )
      case "copy" /: _ =>
        library.copyPlan( header.owner, header.id )
      case _ =>
        if (header.isTransient || !hasChanges)
          header.id.pure[F]
        else
          library.editPlan( header.owner, header.id )
    }

  private def computeAction[X]( hasChanges: Boolean, solution: SolutionHeader[X], actionPath: Uri.Path ): Boolean =
    actionPath match {
      case Actions.compute /: _ => true
      case _                    => hasChanges && solution.isComputed
    }

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

  private def reorderGroup( planId: PlanId, hasChanges: Boolean, outputTab: OutputTab, actionPath: Uri.Path ): F[Unit] =
    ( outputTab, actionPath ) match {
      case ( OutputTab.CustomGroup( ix ), Actions.outputGroupOrder /: segment.Int( row ) /: _ ) =>
        planner.setCustomGroupOrder( planId, ix, row ).whenA( !hasChanges )
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
      inputTab: InputTab,
      outputTab: OutputTab,
      request: Request[F]
  ): OptionT[F, PlanId => F[Unit]] =
    collectInputChanges( planId, inputTab, request ) |+| collectOutputChanges( planId, outputTab, request )

  private def collectInputChanges(
      planId: PlanId,
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
      case OutputTab.Steps =>
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
      model: Model,
      sessionApi: SessionApi[F],
      libraryApi: LibraryApi[F],
      plannerApi: PlannerApi[F]
  ): Application[F] =
    new Application[F]( model, libraryApi, plannerApi, SessionMiddleware[F]( sessionApi ) )

  val cookieSessionIdKey = "session-id"

  object segment {
    trait Segment[A] { self =>
      def unapply( segment: String ): Option[A]

      private[segment] def omap[B]( f: A => Option[B] ): Segment[B] =
        (segment: String) => self.unapply( segment ).flatMap( f )

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
