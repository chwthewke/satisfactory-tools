package net.chwthewke.satisfactorytools

import cats.Eval
import cats.Monoid
import cats.Semigroup
import cats.Show
import cats.Traverse
import cats.data.Chain
import cats.derived.semiauto
import cats.free.Cofree
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.functorFilter._
import cats.syntax.semigroup._
import io.circe.Json
import scala.collection.immutable.SortedMap

object JsonSchema {

  final case class JField[+A]( value: A, optional: Boolean )
  object JField {
    def combineOptions[A: Semigroup]( left: Option[JField[A]], right: Option[JField[A]] ): Option[JField[A]] =
      ( left, right ) match {
        case ( None, None )                   => None
        case ( Some( JField( a, _ ) ), None ) => Some( JField( a, true ) )
        case ( None, Some( JField( a, _ ) ) ) => Some( JField( a, true ) )
        case ( Some( JField( a1, o1 ) ), Some( JField( a2, o2 ) ) ) =>
          Some( JField( a1 |+| a2, o1 || o2 ) )
      }
  }

  sealed abstract class JValue( val typeName: String )
  case object JNull    extends JValue( "null" )
  case object JBoolean extends JValue( "boolean" )
  case object JNumber  extends JValue( "number" )
  case object JString  extends JValue( "string" )

  case class JArray[+A]( items: A, nonEmpty: Boolean )
  object JArray {
    implicit def jArraySemigroup[A]( implicit M: Semigroup[A] ): Semigroup[JArray[A]] = {
      implicit val nonEmptySemigroup: Semigroup[Boolean] = _ && _
      semiauto.semigroup[JArray[A]]
    }
  }

  case class JObject[+A]( fields: SortedMap[String, JField[A]] ) {
    def combine[AA >: A: Semigroup]( other: JObject[AA] ): JObject[AA] =
      JObject(
        (fields.keySet ++ other.fields.keySet).toVector
          .mapFilter( k => JField.combineOptions( fields.get( k ), other.fields.get( k ) ).tupleLeft( k ) )
          .to( SortedMap )
      )
  }
  object JObject {
    implicit def jObjectSemigroup[A]( implicit M: Semigroup[A] ): Semigroup[JObject[A]] =
      Semigroup.instance( ( x, y ) => x.combine( y ) )
  }

  case class SchemasF[+A](
      leaves: Set[JValue],
      asArray: Option[JArray[A]],
      asObject: Option[JObject[A]]
  ) {
    def combine[AA >: A: Monoid]( other: SchemasF[AA] ): SchemasF[AA] =
      SchemasF(
        leaves ++ other.leaves,
        (asArray: Option[JArray[AA]]) |+| other.asArray,
        (asObject: Option[JObject[AA]]) |+| other.asObject
      )
  }

  object SchemasF {
    val jNull: SchemasF[Nothing]    = SchemasF( Set( JNull ), None, None )
    val jNumber: SchemasF[Nothing]  = SchemasF( Set( JNumber ), None, None )
    val jBoolean: SchemasF[Nothing] = SchemasF( Set( JBoolean ), None, None )
    val jString: SchemasF[Nothing]  = SchemasF( Set( JString ), None, None )

    def jArray( a: Vector[Json] ): SchemasF[Vector[Json]] =
      SchemasF( Set.empty, Some( JArray( a, a.nonEmpty ) ), None )

    def jObject( a: Vector[( String, Json )] ): SchemasF[Vector[Json]] =
      SchemasF(
        Set.empty,
        None,
        Some(
          JObject(
            a.map { case ( name, value ) => ( name, JField( Vector( value ), false ) ) }
              .to( SortedMap )
          )
        )
      )

    implicit val schemasTraverse: Traverse[SchemasF] = semiauto.traverse[SchemasF]

    implicit def schemasMonoid[A: Monoid]: Monoid[SchemasF[A]] =
      Monoid.instance(
        SchemasF( Set.empty, None, None ),
        ( x, y ) => x.combine( y )
      )
  }

  type Schemas = Cofree[SchemasF, Unit]

  case class IndLine( offset: Int, line: String ) {
    def indent( n: Int ): IndLine = IndLine( n + offset, line )
    override def toString: String = new String( Array.fill( offset )( ' ' ) ) + line
  }
  object IndLine {
    def apply( line: String ): IndLine      = IndLine( 0, line )
    implicit val indLineShow: Show[IndLine] = Show.fromToString
  }

  def renderSchemas( schemas: Cofree[SchemasF, Unit] ): String =
    Cofree
      .cata( schemas )(
        ( _, node: SchemasF[Chain[IndLine]] ) =>
          Eval.later {
            val valueTypes: Chain[IndLine] =
              if (node.asArray.isEmpty && node.asObject.isEmpty)
                Chain.one( IndLine( node.leaves.map( _.typeName ).mkString( " | " ) ) )
              else
                Chain.fromIterableOnce( node.leaves ).map( l => IndLine( l.typeName ) )

            val arraySchema: Chain[IndLine] =
              node.asArray.foldMap { arr =>
                val header: String = s"<array>${if (arr.nonEmpty) " (non-empty)" else ""}"
                if (arr.items.size > 1)
                  Chain.one( IndLine( header ) ) ++ arr.items.map( _.indent( 2 ) )
                else
                  arr.items.headOption.fold( Chain.one( IndLine( "<empty array>" ) ) )(
                    item => Chain.one( IndLine( s"$header of ${item.line}" ) )
                  )
              }

            val objectSchema: Chain[IndLine] = node.asObject.foldMap(
              obj =>
                Chain.one( IndLine( "<object>" ) ) ++
                  obj.fields.toVector
                    .foldMap {
                      case ( name, JField( value, optional ) ) =>
                        val key: String = s"$name${if (optional) " (opt)" else ""}"
                        if (value.size > 1)
                          Chain.one( IndLine( key ) ) ++ value.map( _.indent( 2 ) )
                        else
                          Chain.one( IndLine( s"$key ${value.headOption.fold( "" )( _.line )}" ) )
                    }
                    .map( _.indent( 2 ) )
            )

            valueTypes ++ arraySchema ++ objectSchema
          }
      )
      .map( _.mkString_( "\n" ) )
      .value

  def extractSchema( jsons: Vector[Json] ): Schemas = Cofree.ana( jsons )( step, _ => () )

  def extractSchema( json: Json ): Schemas = extractSchema( Vector( json ) )

  private def step( jsons: Vector[Json] ): SchemasF[Vector[Json]] = {
    jsons.foldMap(
      json =>
        json.fold(
          SchemasF.jNull,
          _ => SchemasF.jBoolean,
          _ => SchemasF.jNumber,
          _ => SchemasF.jString,
          arr => SchemasF.jArray( arr ),
          obj => SchemasF.jObject( obj.toVector )
        )
    )
  }
}
