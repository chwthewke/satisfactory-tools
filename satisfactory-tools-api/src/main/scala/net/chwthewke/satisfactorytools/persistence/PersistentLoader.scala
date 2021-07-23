package net.chwthewke.satisfactorytools
package persistence

import cats.data.OptionT
import cats.syntax.flatMap._
import doobie._

import loader.Loader
import model.Model

trait PersistentLoader[F[_]] {
  def loadModel: F[Model]
}

object PersistentLoader {

  trait Doobie extends PersistentLoader[ConnectionIO] {
    def loader: Loader[ConnectionIO]

    override def loadModel: ConnectionIO[Model] =
      OptionT
        .liftF( ReadModel.readModel( ModelVersion ) )
        .filter(
          m =>
            m.manufacturingRecipes.nonEmpty &&
              m.items.nonEmpty &&
              m.extractedItems.nonEmpty &&
              m.extractionRecipes.nonEmpty
        )
        .getOrElseF( loader.loadModel.flatTap( WriteModel.writeModel( _, ModelVersion ) ) )
  }

  object Doobie extends Doobie {
    override def loader: Loader[doobie.ConnectionIO] = Loader[ConnectionIO]
  }

}
