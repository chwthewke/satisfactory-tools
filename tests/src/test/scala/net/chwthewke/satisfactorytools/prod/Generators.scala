package net.chwthewke.satisfactorytools
package prod

import cats.effect.unsafe.IORuntime
import cats.syntax.all._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._

import data.Countable
import loader.Loader
import model.Model
import model.Recipe

trait Generators extends model.Generators {

  def genInputs: Gen[SolverInputs] =
    ( genBill, genRecipeList, genOptions, genResourceOptions )
      .mapN( SolverInputs( _, _, _, _ ) )

  def genClockedRecipes( recipes: Vector[Recipe] ): Gen[Vector[ClockedRecipe]] =
    pick[Vector]( recipes )
      .flatMap(
        _.traverse( recipe =>
          ( arbitrary[Short], arbitrary[Float] )
            .mapN( ( am, bc ) => ClockedRecipe.overclocked( Countable( recipe, am & 0xffff ), bc.toDouble ) )
        )
      )

  def genManufacturingRecipes(
      recipes: Vector[Recipe]
  ): Gen[Vector[Countable[Double, Recipe]]] =
    pick[Vector]( recipes )
      .flatMap(
        _.traverse( recipe =>
          arbitrary[Float]
            .map( am => Countable( recipe, am.toDouble ) )
        )
      )

  def genFactory: Gen[Factory] =
    for {
      extractionBlocks    <- genClockedRecipes( model.extractionRecipes.map( _._3 ) )
      manufacturingBlocks <- genManufacturingRecipes( model.manufacturingRecipes )
      itemsIn             <- pick[Vector]( model.items.values.toVector )
      itemsInCountable    <- genCountables( itemsIn )
      itemsOut            <- pick[Vector]( model.items.values.toVector )
      itemsOutCountable   <- genCountables( itemsOut )
    } yield Factory( extractionBlocks, manufacturingBlocks, itemsInCountable, itemsOutCountable )

}

object Generators {

  object Loaded extends Generators {
    override lazy val model: Model =
      Loader.io.loadModel( DataVersionStorage.Update4 ).unsafeRunSync()( IORuntime.global )
  }

}
