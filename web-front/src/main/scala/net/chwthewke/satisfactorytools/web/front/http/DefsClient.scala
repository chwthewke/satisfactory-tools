package net.chwthewke.satisfactorytools
package web
package front
package http

import cats.data.Kleisli
import cats.data.OptionT
import cats.effect.Concurrent
import cats.syntax.show._
import io.circe.Decoder
import org.http4s.Method._
import org.http4s.circe.CirceEntityDecoder._
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.syntax.literals._

import model.Model
import model.ModelVersion
import protocol.ModelVersionId
import web.api.DefsApi

class DefsClient[F[_]: Concurrent] extends DefsApi[Kleisli[F, Client[F], *]] {
  val dsl = new Http4sClientDsl[F] {}
  import dsl._

  Decoder[Vector[( ModelVersionId, ModelVersion )]]

  override def getVersions: Kleisli[F, Client[F], Vector[( ModelVersionId, ModelVersion )]] =
    Kleisli( client => client.expect[Vector[( ModelVersionId, ModelVersion )]]( GET( uri"api" / "models" ) ) )

  override def getModel( version: ModelVersionId ): OptionT[Kleisli[F, Client[F], *], Model] =
    OptionT( Kleisli( (client: Client[F]) => client.expectOption[Model]( GET( uri"api" / "model" / version.show ) ) ) )
}
