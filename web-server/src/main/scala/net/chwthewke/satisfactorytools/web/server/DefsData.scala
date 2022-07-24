package net.chwthewke.satisfactorytools
package web
package server

import cats.data.OptionT
import doobie.ConnectionIO

import model.Model
import model.ModelVersion
import persistence.ReadModel
import protocol.ModelVersionId
import web.api.DefsApi

object DefsData extends DefsApi[ConnectionIO] {
  override def getVersions: ConnectionIO[Vector[( ModelVersionId, ModelVersion )]] =
    ReadModel.getModelVersions

  override def getModel( version: ModelVersionId ): OptionT[ConnectionIO, Model] =
    ReadModel.getModel( version )
}
