package net.chwthewke.satisfactorytools
package web.view

import cats.data.NonEmptyVector
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.functorFilter._
import cats.syntax.show._
import cats.syntax.unorderedFoldable._
import cats.syntax.vector._
import enumeratum.EnumEntry
import scalatags.Text
import scalatags.Text.Tag

import model.Recipe
import prod.ClockedRecipe
import prod.tree.FactoryTree
import prod.tree.Tree
import prod.tree.TreeLoc
import web.forms.Actions
import web.forms.Actions.tree.PushDownForTargetData

object TreeView extends ( ( FactoryTree, Int ) => Tag ) {
  import Text.all._
  import Text.tags2.details
  import Text.tags2.summary

  override def apply( tree: FactoryTree, groupCount: Int ): Tag =
    renderNode( TreeLoc.Root, tree.tree )

  //////////////////////
  /* Commands

    - x Destroy: on whole node, if not root
    - Pull up: on recipe, if not root
    - Push down (1): target (existing children + 1), fraction
    - Push down (2): "for" if recipe in children uses any product
   */

  def renderNode( at: TreeLoc, tree: Tree ): Tag = {
    val children = tree.tailForced

    div(
      at.indices.toNev.fold[Frag]( "Root" )( _.map( _ + 1 ).mkString_( "." ) ),
      at.nonRoot.map( loc =>
        button(
          `class`    := "button is-danger",
          formaction := Actions.tree.destroy( loc ),
          "Destroy"
        )
      ),
      Option.when( at == TreeLoc.Root )(
        button(
          `class`    := "button is-danger",
          formaction := Actions.tree.reset,
          "Reset tree"
        )
      ),
      tree.head.zipWithIndex.map {
        case ( recipe, ix ) =>
          renderRecipe( at, recipe, ix, children )
      },
      children.zipWithIndex.map {
        case ( child, ix ) =>
          renderNode( at.append( ix ), child )
      }
    )
  }

  private def radioId[A <: EnumEntry]( name: String, value: A ): String =
    s"${name}_${value.entryName}"

  private def radio[A <: EnumEntry]( radioName: String, radioValue: A ): Tag =
    input(
      `type` := "radio",
      name   := radioName,
      id     := radioId( radioName, radioValue ),
      value  := radioValue.entryName
    )

  def renderRecipe( at: TreeLoc, recipe: ClockedRecipe, ix: Int, children: Vector[Tree] ): Tag =
    details(
      summary(
        display.flex,
        flexWrap.nowrap,
        div(
          flex    := "1",
          padding := "0.25em",
          f"${recipe.mainProductAmount}%4.3f",
          title := recipe.mainProductAmount.toString
        ), {
          val recipeName  = recipe.recipe.item.displayName
          val altPrefix   = "Alternate: "
          val isAlternate = recipeName.startsWith( altPrefix )
          div(
            flex    := "1",
            padding := "0.25em",
            display.flex,
            flexWrap.nowrap,
            div( recipeName.stripPrefix( altPrefix ), flex := "1" ),
            Option.when( isAlternate )( div( flex := "0 1 auto", "ALT" ) ),
            title := RecipesView.describeRecipe( recipe.recipe.item )
          )
        },
        div(
          flex    := "1",
          padding := "0.25em",
          f"${recipe.machineCount}%3d × ${recipe.machine.displayName}"
        ),
        div(
          flex    := "1",
          padding := "0.25em",
          f"${recipe.mainProductAmountPerUnit}%3.3f / unit @ ${recipe.clockSpeedMillionth / 10000}%3d.${recipe.clockSpeedMillionth % 10000}%04d %%",
          title := recipe.mainProductAmountPerUnit.toString
        ),
        div(
          flex    := "1",
          padding := "0.25em",
          show"${recipe.power} MW"
        )
      ),
      at.nonRoot.map( loc =>
        div(
          button(
            `class` := "button",
            "Pull up",
            formaction := Actions.tree.pullUp( loc, recipe.recipe.item.className )
          )
        )
      ),
      div(
        button(
          `class` := "button",
          "Push down",
          formaction := Actions.tree.pushDown( at, ix, recipe.recipe.item.className )
        ),
        " to ",
        select(
          name := Actions.tree.pushDownTargetDropdown( at, ix ),
          (
            children.indices.map( chIx => ( at.append( chIx ).toString, chIx ) ) :+
              ( ( s"${at.append( children.size )} (new)", children.size ) )
          ).map {
            case ( txt, v ) => option( value := v, txt )
          }
        ),
        radio( Actions.tree.pushDownRadio( at, ix ), Actions.tree.RegularPushDownTypeChoice.All )( checked ),
        label(
          `for` := radioId( Actions.tree.pushDownRadio( at, ix ), Actions.tree.RegularPushDownTypeChoice.All ),
          "all"
        ),
        radio( Actions.tree.pushDownRadio( at, ix ), Actions.tree.RegularPushDownTypeChoice.Fraction ),
        label(
          `for` := radioId( Actions.tree.pushDownRadio( at, ix ), Actions.tree.RegularPushDownTypeChoice.Fraction ),
          "fraction"
        ),
        select(
          name := Actions.tree.pushDownFractionDropdown( at, ix ),
          2.to( 6 ).map( n => option( value := n, s"1/$n" ) )
        ),
        radio( Actions.tree.pushDownRadio( at, ix ), Actions.tree.RegularPushDownTypeChoice.Amount ),
        label(
          `for` := radioId( Actions.tree.pushDownRadio( at, ix ), Actions.tree.RegularPushDownTypeChoice.Amount ),
          "amount"
        ),
        input(
          `type`              := "text",
          attr( "inputmode" ) := "numeric",
          name                := Actions.tree.pushDownAmountInput( at, ix ),
          value               := "0.0"
        )
      ),
      pushDownForControl( at, recipe, ix, children )
    )

  private def pushDownForControl( at: TreeLoc, recipe: ClockedRecipe, ix: Int, children: Vector[Tree] ): Frag = {
    val products = recipe.productsPerMinute.map( _.item.className )
    def asTarget( rec: ClockedRecipe ): Option[Recipe] =
      Some( rec.recipe.item ).filter( r => r.ingredients.exists( ci => products.contains_( ci.item.className ) ) )

    val targets: Option[NonEmptyVector[( Recipe, Int )]] =
      children.zipWithIndex.flatMap {
        case ( sub, subIx ) =>
          sub.head.mapFilter( asTarget ).tupleRight( subIx )
      }.toNev

    targets.map( ts =>
      div(
        button(
          `class` := "button",
          "Push down for",
          formaction := Actions.tree.pushDownFor( at, ix, recipe.recipe.item.className )
        ),
        select(
          name := Actions.tree.pushDownForDropdown( at, ix ),
          ts.toVector.map {
            case ( recipe, subIx ) =>
              option(
                value := PushDownForTargetData( recipe.className, subIx ).renderString,
                s"${recipe.displayName} (${at.append( subIx )})"
              )
          }
        )
      )
    )
  }

}
