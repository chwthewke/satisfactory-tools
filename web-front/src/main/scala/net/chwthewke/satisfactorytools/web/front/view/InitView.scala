package net.chwthewke.satisfactorytools
package web
package front
package view

import outwatch._
import outwatch.dsl._

import model.Model
import vm.InitVm

object InitView {
  def viewLoading( loading: InitVm.Loading ): HtmlVNode =
    div(
      tags.extra.progress(
        maxAttr := "100",
        `class` := "progress",
        `class` := "is-primary"
      ),
      p( "Loading..." )
    )

  def viewLoaded( model: Model ): HtmlVNode =
    div( "TODO" )

  def apply( initVm: InitVm ): HtmlVNode =
    initVm match {
      case loading @ InitVm.Loading( _ ) => viewLoading( loading )
      case InitVm.Loaded( model )        => viewLoaded( model )
    }

}
