package net.chwthewke.satisfactorytools
package web

import cats.data.ValidatedNel
import cats.effect.Async
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.show._
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
import prod.Calculator
import prod.ConstraintSolver
import prod.Factory
import web.state.InputTab
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
    case req @ POST -> Root / "input" / InputTabSegment( dest ) / "from" / InputTabSegment( src ) =>
      respondNavigateInputs( req, src, dest )
    case req @ POST -> Root =>
      respondComputeSolution( req )
  }

  def respondDefault: F[Response[F]] =
    redirect( PageState( defaultInputs, InputTab.BillTab, compute = false ) )

  def respondWithState( state: ValidatedNel[ParseFailure, PageState] ): F[Response[F]] =
    state.fold(
      errs => BadRequest( errs.map( err => s"${err.sanitized} ${err.details}" ).mkString_( "\n" ) ),
      respond
    )

  def respondWithCompute( state: PageState ): F[Response[F]] =
    Ok( View.tabbed( model, state.copy( compute = true ), Some( solve( state.inputs ) ) ) )

  def respondComputeSolution( req: Request[F] ): F[Response[F]] =
    req
      .as[PageState]
      .flatTap( st => logger.info( show"Compute Solution state: $st" ) )
      .flatMap( respondUpdateInputs( req, _, _.selectedInputTab, None, setCompute = true ) )

  def respondNavigateInputs( req: Request[F], src: InputTab, dest: InputTab ): F[Response[F]] =
    req
      .as[PageState]
      .flatTap( st => logger.info( show"Navigate Inputs from $src to $dest state: $st" ) )
      .flatMap( respondUpdateInputs( req, _, _ => src, Some( dest ), setCompute = false ) )

  private def respondUpdateInputs(
      req: Request[F],
      state: PageState,
      getInputTab: PageState => InputTab,
      nextInputTab: Option[InputTab],
      setCompute: Boolean
  ): F[Response[F]] = {
    val inputTab                                                 = getInputTab( state )
    implicit val formDataDecoder: FormDataDecoder[inputTab.Data] = inputTab.decoder( model )

    req
      .as[inputTab.Data]
      .flatMap(
        input =>
          redirect(
            PageState(
              inputTab.stateLens.set( state.inputs )( input ),
              nextInputTab.getOrElse( state.selectedInputTab ),
              state.compute || setCompute
            )
          )
      )
  }

  private def respond( state: PageState ): F[Response[F]] =
    Ok( View.tabbed( model, state, Option.when( state.compute )( solve( state.inputs ) ) ) )

  private def redirect( state: PageState ): F[Response[F]] =
    logger.info( show"Respond with state $state" ) *>
      Found( Location( uri"/".withQueryParam( stateParam, PageState.toBase64( model, state ) ) ) )
//  {
//    logger.info( show"Respond with state $state" ) *>
//      logger.info( show"Respond with state BASE64 ${PageState.toBase64( model, state )}" ) *>
//      Ok( View.tabbed( model, state, Option.when( state.compute )( solve( state.inputs ) ) ) )
//  }

  def solve( inputs: SolverInputs ): Either[String, Factory] =
    Calculator
      .computeFactory( model, inputs, ConstraintSolver )
      .leftMap( err => s"""$err
                          |
                          |<br/>
                          |bill
                          |
                          |${inputs.bill}
                          |
                          |recipes
                          |
                          |${inputs.recipeList.recipes.size} recipes
                          |
                          |options
                          |
                          |${inputs.options} 
                          |
                          |mapOptions
                          |
                          |${inputs.mapOptions} 
                          |""".stripMargin )

  implicit object PageStateQueryParamDecoder extends QueryParamDecoder[PageState] {
    override def decode( value: QueryParameterValue ): ValidatedNel[ParseFailure, PageState] =
      PageState
        .fromBase64( model, value.value )
        .leftMap( err => ParseFailure( s"Bad state", err ) )
        .toValidatedNel
  }

  object InputTabSegment {
    def unapply( segment: String ): Option[InputTab] =
      InputTab.withNameOption( segment )
  }

  object PageStateParam extends OptionalValidatingQueryParamDecoderMatcher[PageState]( stateParam )

}

object Pages {
  val stateParam: String = "state"
}
