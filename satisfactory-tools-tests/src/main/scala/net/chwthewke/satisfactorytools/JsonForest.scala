package net.chwthewke.satisfactorytools

import cats.Monoid
import cats.Show
import cats.instances.string._
import cats.instances.vector._
import cats.syntax.foldable._
import cats.syntax.show._
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Json
//
import model.NativeClass

case class JsonForest( schemas: Vector[JsonSchema] )
object JsonForest {
  def apply( jsonSchema: JsonSchema ): JsonForest = JsonForest( Vector( jsonSchema ) )

  def of( json: Json ): JsonForest = apply( JsonSchema.of( json ) )

  implicit val jsonForestMonoid: Monoid[JsonForest] = new Monoid[JsonForest] {
    override def empty: JsonForest = JsonForest( Vector.empty )

    override def combine( x: JsonForest, y: JsonForest ): JsonForest =
      JsonForest( (x.schemas ++ y.schemas).distinct )
  }

  implicit val jsonForestShow: Show[JsonForest] =
    Show.show( forest => forest.schemas.map( _.show ).intercalate( "\n" ) )

  implicit val jsonForestDecoder: Decoder[JsonForest] =
    Decoder.instance( hc => hc.focus.map( JsonForest.of ).toRight( DecodingFailure( "Failed focus", hc.history ) ) )

  implicit val forestByNativeClassDecoder: Decoder[Map[NativeClass, JsonForest]] = for {
    nativeClass <- Decoder[NativeClass].prepare( _.downField( "NativeClass" ) )
    forest      <- Decoder[JsonForest].prepare( _.downField( "Classes" ) )
  } yield Map( nativeClass -> forest )

}
