package net.chwthewke.satisfactorytools
package web
package front
package view

import cats.Order.catsKernelOrderingForOrder
import cats.syntax.foldable._
import outwatch._
import outwatch.dsl._
import outwatch.dsl.tags.extra.progress
import scala.collection.immutable.SortedSet

import model.Model
import vm.InitVm

object InitView {
  def viewLoading: HtmlVNode =
    div(
      h1( `class` := "title has-text-centered", "Loading" ),
      progress( `class` := "progress is-primary", maxAttr := "100" )
    )

  def viewLoaded( model: Model ): HtmlVNode = div(
    p( "TEST" ),
    div(
      `class` := "buttons",
      model.manufacturingRecipes
        .foldMap( _.products.toList.map( _.item ).to( SortedSet ) )
        .toSeq
        .map { item =>
          button(
            `class` := "button",
            img(
              `class` := "image is-32x32",
              src := s"/icon/${model.version.version}/${item.smallIcon.textureName}.png",
              title := item.displayName
            )
          )
        }
    )
  )

  def apply( initVm: InitVm ): HtmlVNode =
    initVm match {
      case InitVm.Loading( _ )    => viewLoading
      case InitVm.Loaded( model ) => viewLoaded( model )
    }

}
