package net.chwthewke.satisfactorytools
package web
package front
package view

import outwatch._
//import outwatch.dsl._

import model.Model
import vm.InitVm

object InitView {
  def viewLoading( loading: InitVm.Loading ): HtmlVNode = ???

  def viewLoaded( model: Model ): HtmlVNode = ???

  def apply( initVm: InitVm ): HtmlVNode =
    initVm match {
      case loading @ InitVm.Loading( _ ) => viewLoading( loading )
      case InitVm.Loaded( model )        => viewLoaded( model )
    }

}
