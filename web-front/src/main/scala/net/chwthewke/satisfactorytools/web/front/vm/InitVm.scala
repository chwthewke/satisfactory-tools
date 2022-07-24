package net.chwthewke.satisfactorytools
package web.front.vm

import model.Model

sealed trait InitVm

object InitVm {
  case class Loading( model: Option[Model] ) extends InitVm {
    def loaded: InitVm = model.fold[InitVm]( this )( Loaded )
  }

  case class Loaded( model: Model ) extends InitVm
}
