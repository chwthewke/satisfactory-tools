package net.chwthewke.satisfactorytools
package web

import cats.data.ValidatedNel
import cats.effect.Async
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.show._
import cats.syntax.traverse._
import org.http4s.FormDataDecoder
import org.http4s.HttpRoutes
import org.http4s.ParseFailure
import org.http4s.QueryParamDecoder
import org.http4s.QueryParameterValue
import org.http4s.Request
import org.http4s.Response
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.Location
import org.http4s.scalatags._
import org.http4s.syntax.literals._
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import model.Model
import model.SolverInputs
import net.chwthewke.satisfactorytools.web.state.CustomGroupSelection
import prod.Calculator
import prod.ConstraintSolver
import prod.Factory
import web.state.InputTab
import web.state.OutputTab
import web.state.PageState
import web.view.View

case class Pages[F[_]: Async]( model: Model, defaultInputs: SolverInputs ) {

  val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName( "PAGES" )

  import Pages._
  val dsl: Http4sDsl[F] = new Http4sDsl[F] {}
  import dsl._

  implicit val stateDataDecoder: FormDataDecoder[PageState] = PageState.formDataDecoder( model )
  import FormDataDecoder.formEntityDecoder

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root :? PageStateParam( state ) =>
      state.fold( respondDefault )( respondWithState )
    case req @ POST -> Root / "input" / InputTabSegment( dest ) =>
      respondNavigateInputs( req, dest )
    case req @ POST -> Root / "output" / OutputTabSegment( dest ) =>
      respondNavigateOutputs( req, dest )
    case req @ POST -> Root =>
      respondComputeSolution( req )
  }

  def respondDefault: F[Response[F]] =
    redirect( PageState( defaultInputs, InputTab.BillTab, OutputTab.BlocksTab, None, CustomGroupSelection.empty ) )

  def respondWithState( state: ValidatedNel[ParseFailure, PageState] ): F[Response[F]] =
    state.fold(
      errs => BadRequest( errs.map( err => s"${err.sanitized} ${err.details}" ).mkString_( "\n" ) ),
      respond
    )

  def respondComputeSolution( req: Request[F] ): F[Response[F]] =
    req
      .as[PageState]
      .flatMap( respondUpdateInputs( req, _, setCompute = true ) )

  def respondNavigateInputs( req: Request[F], dest: InputTab ): F[Response[F]] =
    req
      .as[PageState]
      .flatMap( respondUpdateInputs( req, _, newInputTab = Some( dest ) ) )

  def respondNavigateOutputs( req: Request[F], dest: OutputTab ): F[Response[F]] =
    req
      .as[PageState]
      .flatMap( respondUpdateInputs( req, _, newOutputTab = Some( dest ) ) )

  private def respondUpdateInputs(
      req: Request[F],
      state: PageState,
      newInputTab: Option[InputTab] = None,
      newOutputTab: Option[OutputTab] = None,
      setCompute: Boolean = false
  ): F[Response[F]] = {
    val inputTab                                                  = state.selectedInputTab
    implicit val inputDataDecoder: FormDataDecoder[inputTab.Data] = inputTab.decoder( model )

    for {
      newCustomGroups <- state.selectedOutputTab.customGroupsFormDataDecoder( model ).traverse { implicit dec =>
                          req.as[CustomGroupSelection]
                        }
      input <- req.as[inputTab.Data]
      newInputs = inputTab.stateLens.set( state.inputs )( input )
      response <- redirect(
                   PageState(
                     newInputs,
                     newInputTab.getOrElse( state.selectedInputTab ),
                     newOutputTab.getOrElse( state.selectedOutputTab ),
                     state.factory.void.orElse( Option.when( setCompute )( () ) ).as( solve( newInputs ) ),
                     newCustomGroups.getOrElse( state.customGroupSelection )
                   )
                 )
    } yield response
  }

  private def respond( state: PageState ): F[Response[F]] =
    Ok( View.tabbed( model, state ) )

  private def redirect( state: PageState ): F[Response[F]] =
    logger.info( show"Respond with state $state" ) *>
      PageState
        .toBase64( model, state )
        .leftMap( Error( _ ) )
        .liftTo[F]
        .flatMap( st => Found( Location( uri"/".withQueryParam( stateParam, st ) ) ) )

  def solve( inputs: SolverInputs ): Either[String, Factory] =
    Calculator.computeFactory( model, inputs, ConstraintSolver )

  implicit object PageStateQueryParamDecoder extends QueryParamDecoder[PageState] {
    override def decode( value: QueryParameterValue ): ValidatedNel[ParseFailure, PageState] =
      PageState
        .fromBase64( model, value.value )
        .leftMap( err => ParseFailure( s"Bad state", err ) )
        .toValidatedNel
  }

  object PageStateParam extends OptionalValidatingQueryParamDecoderMatcher[PageState]( stateParam )

  object InputTabSegment {
    def unapply( segment: String ): Option[InputTab] =
      InputTab.withNameOption( segment )
  }

  object OutputTabSegment {
    def unapply( segment: String ): Option[OutputTab] = OutputTab.withId( segment )
  }

}

object Pages {
  val stateParam: String = "state"
}
