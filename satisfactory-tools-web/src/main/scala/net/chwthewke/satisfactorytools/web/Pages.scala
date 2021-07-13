package net.chwthewke.satisfactorytools
package web

import cats.data.ValidatedNel
import cats.effect.Async
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.option._
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
import org.typelevel.vault.Key

import model.Bill
import model.Model
import model.Options
import model.RecipeList
import model.ResourceOptions
import net.chwthewke.satisfactorytools.web.session.ProductionPlannerSession
import net.chwthewke.satisfactorytools.web.session.StoredFactory
import prod.Calculator
import prod.ConstraintSolver
import prod.Factory
import prod.SolverInputs
import web.protocol.Forms
import web.state.CustomGroupSelection
import web.state.InputTab
import web.state.OutputTab
import web.state.PageState
import web.view.CompareView
import web.view.View

case class Pages[F[_]]( sessionKey: Key[ProductionPlannerSession[F]], model: Model, defaultInputs: SolverInputs )(
    implicit F: Async[F]
) {

  val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName( "PAGES" )

  import Pages._
  val dsl: Http4sDsl[F] = new Http4sDsl[F] {}
  import dsl._

  implicit val stateDataDecoder: FormDataDecoder[PageState] = PageState.formDataDecoder( model, Forms.state )
  import FormDataDecoder.formEntityDecoder

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root :? PageStateParam( state )                   => state.fold( respondDefault )( respondWithState )
    case req @ POST -> Root / "input" / InputTabSegment( dest )   => respondNavigateInputs( req, dest )
    case req @ POST -> Root / "output" / OutputTabSegment( dest ) => respondNavigateOutputs( req, dest )
    case req @ POST -> Root                                       => respondComputeSolution( req )
    case req @ POST -> Root / "group_dec"                         => respondRemoveCustomGroup( req )
    case req @ POST -> Root / "group_inc"                         => respondAddCustomGroup( req )
    case req @ POST -> Root / "upgrade"                           => upgrade( req )
    case GET -> Root / "compare"                                  => newCompare
    case req @ POST -> Root / "compare"                           => compare( req )
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

  def respondAddCustomGroup( req: Request[F] ): F[Response[F]] =
    req
      .as[PageState]
      .flatMap( respondUpdateInputs( req, _, customGroupsAdj = 1 ) )

  def respondRemoveCustomGroup( req: Request[F] ): F[Response[F]] =
    req
      .as[PageState]
      .flatMap( respondUpdateInputs( req, _, customGroupsAdj = -1 ) )

  private def respondUpdateInputs(
      req: Request[F],
      state: PageState,
      newInputTab: Option[InputTab] = None,
      newOutputTab: Option[OutputTab] = None,
      customGroupsAdj: Int = 0,
      setCompute: Boolean = false
  ): F[Response[F]] = {
    val inputTab                                                  = state.selectedInputTab
    implicit val inputDataDecoder: FormDataDecoder[inputTab.Data] = inputTab.decoder( model )

    for {
      selectedCustomGroups <- state.selectedOutputTab
                               .customGroupsFormDataDecoder( model )
                               .fold( state.customGroupSelection.pure[F] ) { implicit dec =>
                                 req.as[CustomGroupSelection]
                               }
      input <- req.as[inputTab.Data]
      newInputs = inputTab.stateLens.set( state.inputs )( input )
      session   = req.attributes.lookup( sessionKey ).getOrElse( ProductionPlannerSession.Null[F]() )
      now <- F.realTimeInstant
      newFactory <- if (setCompute || (newInputs != state.inputs && state.factory.isDefined)) {
                     val solution = solve( newInputs )
                     solution
                       .traverse_( factory => session.pushHistory( StoredFactory( now, newInputs, factory ) ) )
                       .as( Some( solution ) )
                   } else
                     F.pure( state.factory )
      response <- redirect(
                   PageState(
                     newInputs,
                     newInputTab.getOrElse( state.selectedInputTab ),
                     newOutputTab.getOrElse( state.selectedOutputTab ),
                     newFactory,
                     selectedCustomGroups.copy( count = selectedCustomGroups.count + customGroupsAdj )
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

  val newCompare: F[Response[F]] =
    Ok( CompareView( model, None, None ) )

  def compare( req: Request[F] ): F[Response[F]] = {
    implicit val dualStateDecoder: FormDataDecoder[( Option[PageState], Option[PageState] )] =
      (
        PageState.formDataDecoderOpt( model, Forms.compareBefore ),
        PageState.formDataDecoderOpt( model, Forms.compareAfter )
      ).tupled

    Ok(
      req
        .as[( Option[PageState], Option[PageState] )]
        .map { case ( before, after ) => CompareView( model, before, after ) }
    )
  }

  def upgrade( req: Request[F] ): F[Response[F]] = {
    implicit val billDecoder: FormDataDecoder[Bill]                       = Forms.bill( model )
    implicit val recipeListDecoder: FormDataDecoder[RecipeList]           = Forms.recipeList( model )
    implicit val optionsDecoder: FormDataDecoder[Options]                 = Forms.options
    implicit val resourceOptionsDecoder: FormDataDecoder[ResourceOptions] = Forms.resourceOptions( model )

    implicit val inputTabDecoder: FormDataDecoder[InputTab] =
      FormDataDecoder
        .field[String]( "input_tab" )
        .mapValidated(
          id =>
            InputTab
              .withNameOption( id )
              .toValidNel( ParseFailure( "", s"Unknown input tab $id" ) )
        )

    implicit val outputTabDecoder: FormDataDecoder[OutputTab] =
      FormDataDecoder
        .field[String]( "output_tab" )
        .mapValidated(
          id =>
            OutputTab
              .withId( id )
              .toValidNel( ParseFailure( "", s"Unknown output tab $id" ) )
        )

    implicit val customGroupsDecoder: FormDataDecoder[CustomGroupSelection] = Forms.customGroups( model )

    (
      req.as[Bill],
      req.as[RecipeList],
      req.as[Options],
      req.as[ResourceOptions],
      req.as[InputTab],
      req.as[OutputTab],
      req.as[CustomGroupSelection]
    ).mapN { ( bill, recipes, options, resourceOptions, inputTab, outputTab, customGroups ) =>
        val inputs = SolverInputs( bill, recipes, options, resourceOptions )
        PageState(
          inputs,
          inputTab,
          outputTab,
          Some( solve( inputs ) ),
          customGroups
        )
      }
      .flatMap( redirect )

  }

  def solve( inputs: SolverInputs ): Either[String, Factory] =
    Calculator.computeFactory( model, inputs, ConstraintSolver )

  implicit object PageStateQueryParamDecoder extends QueryParamDecoder[PageState] {
    override def decode( value: QueryParameterValue ): ValidatedNel[ParseFailure, PageState] =
      PageState.validate( model, value.value )
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
