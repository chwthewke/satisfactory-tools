package net.chwthewke.satisfactorytools
package web.app

import cats.Order
import cats.effect.Async
import cats.effect.Concurrent
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.semigroupk._
import cats.syntax.show._
import cats.syntax.traverse._
import enumeratum.Enum
import enumeratum.EnumEntry
import org.http4s.ContextRequest
import org.http4s.ContextRoutes
import org.http4s.FormDataDecoder
import org.http4s.HttpRoutes
import org.http4s.Request
import org.http4s.Response
import org.http4s.Uri
import org.http4s.dsl.Http4sDsl
import org.http4s.dsl.RequestDsl
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
import protocol.PlanId
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
)( implicit F: Concurrent[F] ) {

  private implicit val dsl: Http4sDsl[F] = new Http4sDsl[F] {}
  import dsl._

  import Application.Compute
  import Application.Outcome
  import Application.segment

  val libraryRoutes: ContextRoutes[Session, F] = ContextRoutes.of[Session, F] {
    case ContextRequest( session, GET -> Root ) =>
      library.getAllPlans( session.userId ).flatMap( plans => Ok( LibraryView.viewAllPlans( plans ) ) )

    case ContextRequest( session, POST -> Root / "new" ) =>
      planner
        .newPlan( session.userId, Options.default, model.defaultResourceOptions )
        .flatMap( redirect( _, InputTab.Bill, OutputTab.Steps ) )

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
      planAction( planId, inputTab, outputTab, rest, req )

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

  private def planAction(
      planId: PlanId,
      inputTab: InputTab,
      outputTab: OutputTab,
      path: Uri.Path,
      request: Request[F]
  ): F[Response[F]] =
    planner
      .getPlanHeader( planId )
      .toRightF( NotFound() )
      .semiflatMap(
        header =>
          recordInput( planId, inputTab, request ) *>
            recordOutput( planId, outputTab, request ) *>
            // TODO later normal recompute decision (has changed), see Plans#setBill
            reviewPlan( planId, inputTab, outputTab, header.solution, Outcome.of( path ) )
      )
      .merge

  private def recordInput(
      planId: PlanId,
      inputTab: InputTab,
      request: Request[F]
  ): F[Unit] =
    inputTab match {
      case InputTab.Bill =>
        decode( request )( Decoders.bill( model ) ).flatMap( planner.setBill( planId, _ ) )
      case InputTab.Recipes =>
        decode( request )( Decoders.recipeList( model ) ).flatMap( planner.setRecipeList( planId, _ ) )
      case InputTab.Options =>
        decode( request )( Decoders.options ).flatMap( planner.setOptions( planId, _ ) )
      case InputTab.ResourceOptions =>
        decode( request )( Decoders.resourceOptions( model ) ).flatMap( planner.setResourceOptions( planId, _ ) )
    }

  private def recordOutput(
      planId: PlanId,
      outputTab: OutputTab,
      request: Request[F]
  ): F[Unit] =
    outputTab match {
      case OutputTab.Steps =>
        decode( request )( Decoders.customGroups ).flatMap( planner.setCustomGroupSelection( planId, _ ) )
      case _ => F.unit
    }

  private def decode[A]( request: Request[F] )( implicit formDataDecoder: FormDataDecoder[A] ): F[A] = {
    import FormDataDecoder.formEntityDecoder
    request.as[A]
  }

  private def reviewPlan[X](
      planId: PlanId,
      inputTab: InputTab,
      outputTab: OutputTab,
      solution: SolutionHeader[X],
      outcome: Outcome
  ): F[Response[F]] =
    planner
      .computePlan( planId )
      .whenA( outcome.compute == Compute.Force || outcome.compute == Compute.Update && solution.isComputed ) *>
      planner.addCustomGroup( planId ).whenA( outcome.groupAdjustment > 0 ) *>
      planner.removeCustomGroup( planId ).whenA( outcome.groupAdjustment < 0 ) *>
      redirect( planId, outcome.newInput.getOrElse( inputTab ), outcome.newOutput.getOrElse( outputTab ) )

  private def redirect(
      planId: PlanId,
      inputTab: InputTab,
      outputTab: OutputTab
  ): F[Response[F]] =
    Found( Location( uri"/plan" / planId.show / Actions.input( inputTab ) / Actions.output( outputTab ) / "" ) )

  val routes: HttpRoutes[F] = sessionMiddleware( libraryRoutes <+> planRoutes )

}

object Application {

  sealed trait Compute extends EnumEntry
  object Compute extends Enum[Compute] {
    final case object Pass   extends Compute
    final case object Update extends Compute
    final case object Force  extends Compute

    override val values: IndexedSeq[Compute] = findValues

    implicit val computeOrder: Order[Compute] = Order.by( indexOf )
  }

  sealed abstract class Outcome(
      val newInput: Option[InputTab],
      val newOutput: Option[OutputTab],
      val groupAdjustment: Int,
      val compute: Compute
  ) extends Product

  object Outcome {
    final case class ToInput( inputTab: InputTab )    extends Outcome( Some( inputTab ), None, 0, Compute.Update )
    final case class ToOutput( outputTab: OutputTab ) extends Outcome( None, Some( outputTab ), 0, Compute.Update )
    final case object AddGroup                        extends Outcome( None, None, 1, Compute.Update )
    final case object RemoveGroup                     extends Outcome( None, None, -1, Compute.Update )
    final case object ForceCompute                    extends Outcome( None, None, 0, Compute.Force )
    final case object Neutral                         extends Outcome( None, None, 0, Compute.Pass )

    def of( path: Uri.Path )( implicit dsl: RequestDsl ): Outcome = {
      import dsl._

      val outcome = path match {
        case "input" /: Actions.input( tab ) /: _   => ToInput( tab )
        case "output" /: Actions.output( tab ) /: _ => ToOutput( tab )
        case "compute" /: _                         => ForceCompute
        case "group_dec" /: _                       => RemoveGroup
        case "group_inc" /: _                       => AddGroup
        case _                                      => Neutral
      }

      println( s"$path => $outcome" )

      outcome
    }
  }

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
