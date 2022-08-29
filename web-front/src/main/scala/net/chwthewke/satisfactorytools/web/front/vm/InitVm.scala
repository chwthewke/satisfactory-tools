package net.chwthewke.satisfactorytools
package web.front.vm

import model.Model

sealed trait InitVm {
  def withModel( model: Model ): InitVm = this match {
    case InitVm.Loading( _ )    => InitVm.Loaded( model )
    case l @ InitVm.Loaded( _ ) => l
  }
}

object InitVm {
  def init: InitVm = Loading( None )

  case class Loading( model: Option[Model] ) extends InitVm {
    def loaded: InitVm = model.fold[InitVm]( this )( Loaded )
  }

  case class Loaded( model: Model ) extends InitVm
}
