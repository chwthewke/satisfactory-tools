package net.chwthewke.satisfactorytools
package prod

import cats.Show
import cats.syntax.all._

import data.Countable
import data.Item
import model.Recipe

final case class Factory(
    extraction: Vector[ClockedRecipe],
    manufacturing: Vector[Countable[Double, Recipe]],
    extraInputs: Vector[Countable[Double, Item]],
    extraOutputs: Vector[Countable[Double, Item]]
) {
  def manufacturingRecipes: Vector[ClockedRecipe] = manufacturing.map( ClockedRecipe.roundUp )
  def allRecipes: Vector[ClockedRecipe]           = extraction ++ manufacturingRecipes
}

object Factory {
  private def showClockedRecipe( cr: ClockedRecipe ) =
    f"${cr.machineCount}%d ${cr.recipe.item.displayName} @ ${cr.clockSpeed}%.4f"

  private def showAmount( amount: Double ) = f"$amount%4.3f"
  private def showCountables[A]( countables: Vector[Countable[Double, A]] )( s: A => String ): String =
    countables
      .map { case Countable( item, amount ) => show"${showAmount( amount )} ${s( item )}" }
      .mkString( "\n" )

  implicit val factoryShow: Show[Factory] = Show.show( factory => //
    show"""EXTRACTION
          |${factory.extraction.map( showClockedRecipe ).mkString( "\n" )}
          |
          |MANUFACTURING
          |${showCountables( factory.manufacturing )( _.displayName )}
          |
          |EXTRA INPUTS
          |${showCountables( factory.extraInputs )( _.displayName )}
          |
          |EXTRA OUTPUTS
          |${showCountables( factory.extraOutputs )( _.displayName )}
          |""".stripMargin
  )
}
