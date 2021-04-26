package net.chwthewke.satisfactorytools
package web

import cats.Monad
import cats.data.ValidatedNel
import cats.effect.Sync
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import org.http4s.FormDataDecoder
import org.http4s.HttpRoutes
import org.http4s.ParseFailure
import org.http4s.QueryParamDecoder
import org.http4s.QueryParameterValue
import org.http4s.Response
import org.http4s.dsl.Http4sDsl
import org.http4s.scalatags._

import model.Model
import model.SolverInputs
import prod.Calculator
import prod.ConstraintSolver
import prod.Factory
import web.protocol.SolverInputsCodec
import web.view.View

case class Pages[F[_]: Sync: Monad]( model: Model, defaultInputs: SolverInputs ) {

  import Pages._
  val dsl: Http4sDsl[F] = new Http4sDsl[F] {}
  import dsl._

  implicit val formDataEntityDecoder: FormDataDecoder[SolverInputs] = SolverInputsCodec.formDataDecoder( model )
  import FormDataDecoder.formEntityDecoder

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root :? PageStateParam( state ) =>
      state.fold( respondDefault )( respondWithState )
    case req @ POST -> Root =>
      req.as[SolverInputs].flatMap( respondWithFormInput )
  }

  def respondDefault: F[Response[F]] = Ok( View( model, defaultInputs, defaultInputs, None ) )

  def respondWithState( state: ValidatedNel[ParseFailure, SolverInputs] ): F[Response[F]] =
    state.fold(
      errs => BadRequest( errs.map( err => s"${err.sanitized} ${err.details}" ).mkString_( "\n" ) ),
      inputs => Ok( View( model, defaultInputs, inputs, Some( solve( inputs ) ) ) )
    )

  def respondWithFormInput( inputs: SolverInputs ): F[Response[F]] =
    Ok( View( model, defaultInputs, inputs, Some( solve( inputs ) ) ) )

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

  implicit object PageStateQueryParamDecoder extends QueryParamDecoder[SolverInputs] {
    override def decode( value: QueryParameterValue ): ValidatedNel[ParseFailure, SolverInputs] =
      SolverInputsCodec
        .fromBase64( model, value.value )
        .leftMap( err => ParseFailure( s"Bad state", err ) )
        .toValidatedNel
  }

  object PageStateParam extends OptionalValidatingQueryParamDecoderMatcher[SolverInputs]( stateParam )

}

object Pages {
  val stateParam: String = "state"
}
