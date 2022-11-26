package net.chwthewke.satisfactorytools
package web.front

import cats.effect.std.Queue

import data.Countable
import data.Item
import model.ModelVersion
import model.{Model => GameModel}
import protocol.ModelVersionId
import web.api.DefsApi

class QueueMain[F[_]]( actions: Queue[F, QueueMain.Action], defsApi: DefsApi[F] ) {
  import QueueMain._

  def runAction( action: Action ): Model => Model = identity // TODO

}

object QueueMain {
  case class Model( state: State, viewState: ViewState )

  case class State(
      gameModelVersions: Vector[( ModelVersionId, ModelVersion )],
      gameModel: Option[GameModel],
      bill: Vector[Countable[Double, Item]]
  )

  case class ViewState( itemsPaneIsOpen: Boolean )

  sealed trait Action

}
