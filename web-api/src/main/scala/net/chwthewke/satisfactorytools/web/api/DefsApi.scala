package net.chwthewke.satisfactorytools
package web
package api

import cats.data.OptionT
import cats.~>

import model.Model
import model.ModelVersion
import net.chwthewke.satisfactorytools.protocol.ModelVersionId

// TODO this is just ModelApi... what gives?
trait DefsApi[F[_]] { self =>
  def getVersions: F[Vector[( ModelVersionId, ModelVersion )]]

  def getModel( version: ModelVersionId ): OptionT[F, Model]

  def mapK[G[_]]( f: F ~> G ): DefsApi[G] = new DefsApi[G] {
    override def getVersions: G[Vector[( ModelVersionId, ModelVersion )]] = f( self.getVersions )

    override def getModel( version: ModelVersionId ): OptionT[G, Model] = self.getModel( version ).mapK( f )
  }
}
