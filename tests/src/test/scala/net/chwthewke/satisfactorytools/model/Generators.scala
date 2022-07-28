package net.chwthewke.satisfactorytools
package model

import alleycats.std.map._
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.traverse._
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._

import data.ClassName

trait Generators extends data.Generators {
  def model: Model

  def genResourceNodes: Gen[Map[ExtractorType, Map[ClassName, ResourceDistrib]]] =
    model.defaultResourceOptions.resourceNodes
      .traverse( _.traverse {
        case ResourceDistrib( impureNodes, normalNodes, pureNodes ) =>
          ( Gen.choose( 0, impureNodes ), Gen.choose( 0, normalNodes ), Gen.choose( 0, pureNodes ) )
            .mapN( ResourceDistrib( _, _, _ ) )
      } )

  def genResourceWeights: Gen[ResourceWeights] =
    model.extractedItems
      .traverse( it => Gen.choose( 0, 2 * ResourceWeights.range ).tupleLeft( it.className ) )
      .map( v => ResourceWeights( v.filter( _._2 != ResourceWeights.range ).toMap ) )

  def genResourceOptions: Gen[ResourceOptions] =
    ( genResourceNodes, genResourceWeights )
      .mapN( ResourceOptions( _, _ ) )

  def genOptions: Gen[Options] =
    (
      Gen.oneOf( Options.Belt.values ),
      Gen.oneOf( Options.Pipe.values ),
      Gen.oneOf( Options.Miner.values ),
      Gen.oneOf( Options.ClockSpeed.values ),
      pick[Set]( ExtractorType.values ),
      pick[Set]( Seq[ExtractorType]( ExtractorType.OilPump, ExtractorType.WaterPump ) )
    ).mapN( Options( _, _, _, _, _, _ ) )

  def genRecipeList: Gen[RecipeList] =
    pick[Vector]( model.manufacturingRecipes )
      .map( _.sortBy( model.manufacturingRecipes.indexOf ) )
      .map( RecipeList( _ ) )

  def genBill: Gen[Bill] =
    genCountables( Gen.choose( 0, 40 ).flatMap( Gen.pick( _, model.items.values.toSeq ) ).map( _.toVector ) )
      .map( Bill( _ ) )

}
