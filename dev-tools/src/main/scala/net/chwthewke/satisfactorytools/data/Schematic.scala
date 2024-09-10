package net.chwthewke.satisfactorytools
package data

import atto._
import Atto._
import atto.Parser
import cats.Show
import cats.data.OptionT
import cats.syntax.all._
import io.circe.Decoder

case class Schematic(
    className: ClassName,
    displayName: String,
    `type`: SchematicType,
    cost: Vector[Countable[Double, ClassName]],
    techTier: Int,
    requireAllDependencies: Boolean,
    schematicDependencies: Vector[ClassName /*Schematic*/ ],
    unlocks: Vector[ClassName /*Recipe*/ ]
)

object Schematic {

  implicit val schematicShow: Show[Schematic] =
    Show.show { s =>
      val depsTag: String = "" // if (s.requireAllDependencies) "ALL OF " else "ONE OF "
      show"""${s.displayName} # ${s.className}
            |  type: ${s.`type`}
            |  tier: ${s.techTier}
            |  cost: ${s.cost.mkString_( ", " )}
            |  deps: $depsTag${s.schematicDependencies.mkString_( ", " )}
            |  unlocks: ${s.unlocks.mkString_( ", " )}
            |""".stripMargin
    }

  import Parsers.ParserOps

  private val schematicDependencyClass: ClassName =
    ClassName( "BP_SchematicPurchasedDependency_C" )

  private val recipeUnlockClass: ClassName =
    ClassName( "BP_UnlockRecipe_C" )

  private val itemCostParser: Parser[Vector[Countable[Double, ClassName]]] =
    Parsers.countableList.map( _.toList.toVector ) | Atto.ok( Vector.empty )

  private val classListParser: Parser[Vector[ClassName]] =
    Parsers.bpGeneratedClassList | Atto.ok( Vector.empty )

  private def classListDecoder( matchingClass: ClassName, classListField: String ): Decoder[Option[Vector[ClassName]]] =
    Decoder.instance( c =>
      OptionT
        .liftF( c.get[ClassName]( "Class" ) )
        .filter( _ == matchingClass )
        .flatMapF( _ =>
          c.get[Option[Vector[ClassName]]]( classListField )( Decoder.decodeOption( classListParser.decoder ) )
        )
        .value
    )

  private val dependenciesDecoder: Decoder[( Vector[ClassName], Boolean )] =
    Decoder
      .decodeVector(
        OptionT( classListDecoder( schematicDependencyClass, "mSchematics" ) )
          .semiflatMap( d =>
            Parsers.booleanString.decoder
              .prepare( _.downField( "mRequireAllSchematicsToBePurchased" ) )
              .map( req => ( d, req ) )
          )
          .value
      )
      .map( _.flattenOption )
      .emap { depBlocks =>
        if (depBlocks.size > 1)
          Left( "Multiple mSchematicDependencies items" )
        else
          Right( depBlocks.headOption.getOrElse( ( Vector.empty, false ) ) )
      }

  private val unlocksDecoder: Decoder[Vector[ClassName]] =
    Decoder
      .decodeVector( classListDecoder( recipeUnlockClass, "mRecipes" ) )
      .map( v => v.mapFilter( identity ).combineAll )

  implicit val schematicDecoder: Decoder[Schematic] = Decoder.instance { c =>
    (
      c.get[ClassName]( "ClassName" ),
      c.get[String]( "mDisplayName" ),
      c.get[SchematicType]( "mType" ),
      c.get( "mCost" )( itemCostParser.decoder ),
      c.get[Int]( "mTechTier" ),
      c.get( "mSchematicDependencies" )( dependenciesDecoder ),
      c.get( "mUnlocks" )( unlocksDecoder )
    ).mapN { ( cn, dn, typ, c, tier, deps, unlocks ) =>
      Schematic(
        cn,
        dn,
        typ,
        c,
        tier,
        deps._2,
        deps._1,
        unlocks
      )
    }

  }
}
