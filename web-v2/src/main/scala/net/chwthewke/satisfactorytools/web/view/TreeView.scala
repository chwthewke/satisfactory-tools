package net.chwthewke.satisfactorytools
package web.view

import cats.syntax.foldable._
import cats.syntax.show._
import cats.syntax.vector._
import scalatags.Text
import scalatags.Text.Tag

import prod.ClockedRecipe
import prod.adv.tree.FactoryTree
import prod.adv.tree.Tree
import prod.adv.tree.TreeLoc

object TreeView extends ( ( FactoryTree, Int ) => Tag ) {
  import Text.all._

  override def apply( tree: FactoryTree, groupCount: Int ): Tag =
    renderNode( TreeLoc.Root, tree.tree )

  def renderNode( at: TreeLoc, tree: Tree ): Tag = {
    val children = tree.tailForced

    div(
      at.indices.toNev.fold[Frag]( "Root" )( _.map( _ + 1 ).mkString_( "." ) ),
      tree.head.map( renderRecipe( at, _, children ) ),
      children.zipWithIndex.map {
        case ( child, ix ) =>
          renderNode( at.append( ix ), child )
      }
    )
  }

  def renderRecipe( at: TreeLoc, recipe: ClockedRecipe, children: Vector[Tree] ): Tag = {
    div(
      div(
        display.flex,
        flexWrap.nowrap,
        div(
          flex := "1",
          f"${recipe.mainProductAmount}%4.3f",
          title := recipe.mainProductAmount.toString
        ), {
          val recipeName  = recipe.recipe.item.displayName
          val altPrefix   = "Alternate: "
          val isAlternate = recipeName.startsWith( altPrefix )
          div(
            flex := "1",
            display.flex,
            flexWrap.nowrap,
            div( recipeName.stripPrefix( altPrefix ), flex := "1" ),
            Option.when( isAlternate )( div( flex := "0 1 auto", "ALT" ) ),
            title := RecipesView.describeRecipe( recipe.recipe.item )
          )
        },
        div(
          flex := "1",
          f"${recipe.machineCount}%3d × ${recipe.machine.displayName}"
        ),
        div(
          flex := "1",
          f"${recipe.mainProductAmountPerUnit}%3.3f / unit @ ${recipe.clockSpeedMillionth / 10000}%3d.${recipe.clockSpeedMillionth % 10000}%04d %%",
          title := recipe.mainProductAmountPerUnit.toString
        ),
        div(
          flex := "1",
          show"${recipe.power} MW"
        )
      )
    )
  }

}
