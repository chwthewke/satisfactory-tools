package net.chwthewke.satisfactorytools
package web
package front
package view

import outwatch._
import outwatch.dsl._
import outwatch.dsl.tags.extra.progress

import model.Model
import vm.InitVm

object InitView {
  def viewLoading: HtmlVNode =
    div(
      h1( `class` := "title has-text-centered", "Loading" ),
      progress( `class` := "progress is-primary", maxAttr := "100" )
    )

  def viewLoaded( model: Model ): HtmlVNode = div( "TODO" )

  def apply( initVm: InitVm ): HtmlVNode =
    initVm match {
      case InitVm.Loading( _ )    => viewLoading
      case InitVm.Loaded( model ) => viewLoaded( model )
    }

}
