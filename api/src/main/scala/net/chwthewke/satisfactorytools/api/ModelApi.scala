package net.chwthewke.satisfactorytools
package api

import cats.data.OptionT
import cats.~>

import model.Model
import model.ModelVersion
import protocol.ModelVersionId

trait ModelApi[F[_]] { self =>
  def getModelVersions: F[Vector[( ModelVersionId, ModelVersion )]]

  def getModel( version: ModelVersionId ): OptionT[F, Model]

  def mapK[G[_]]( f: F ~> G ): ModelApi[G] = new ModelApi[G] {
    override def getModelVersions: G[Vector[( ModelVersionId, ModelVersion )]] =
      f( self.getModelVersions )

    override def getModel( version: ModelVersionId ): OptionT[G, Model] =
      self.getModel( version ).mapK( f )
  }
}
