package net.chwthewke.satisfactorytools

import cats.Show
import cats.syntax.foldable._
import cats.syntax.show._
import io.circe.Json

sealed trait JsonSchema extends Product
object JsonSchema {

  final case class JsonObject( keys: Vector[( String, JsonSchema )] ) extends JsonSchema
  final case class JsonArray( values: Vector[JsonSchema] )            extends JsonSchema
  final case class JsonType( typ: String )                            extends JsonSchema

  def of( json: Json ): JsonSchema =
    json.fold(
      JsonType( "null" ),
      _ => JsonType( "boolean" ),
      _ => JsonType( "number" ),
      _ => JsonType( "string" ),
      arr => JsonArray( arr.foldMap( j => JsonForest( of( j ) ) ).schemas ),
      obj => JsonObject( obj.toVector.map { case ( k, v ) => ( k, of( v ) ) } )
    )

  def showJsonSchema( ind: String, schema: JsonSchema ): String =
    schema match {
      case JsonObject( keys ) =>
        show"""$ind{
              |${keys
                .map { case ( k, v ) => show"$ind  '$k':\n${showJsonSchema( s"  $ind", v )}" }
                .intercalate( "\n" )}
              |$ind}""".stripMargin
      case JsonArray( values ) =>
        show"""$ind{
              |${values.map( v => show"$ind  ${showJsonSchema( s"  $ind", v )}" ).intercalate( "\n" )}
              |$ind}""".stripMargin
      case JsonType( typ ) => ind + typ.show
    }

  implicit val jsonSchemaShow: Show[JsonSchema] = Show.show { showJsonSchema( "", _ ) }

}
