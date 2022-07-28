package net.chwthewke.satisfactorytools
package web.front.vm

import model.Model

sealed trait InitVm {
  def modelReceived( model: Option[Model] ): InitVm
}

object InitVm {
  val init = Loading( None )

  case class Loading( model: Option[Model] ) extends InitVm {
    override def modelReceived( model: Option[Model] ): InitVm = copy( model = model ).loaded

    def loaded: InitVm = model.fold[InitVm]( this )( Loaded )
  }

  case class Loaded( model: Model ) extends InitVm {
    override def modelReceived( model: Option[Model] ): InitVm = this
  }
}
