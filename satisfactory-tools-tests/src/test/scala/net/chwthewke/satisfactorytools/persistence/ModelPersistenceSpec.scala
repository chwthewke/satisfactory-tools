package net.chwthewke.satisfactorytools
package persistence

import cats.effect.IO
import doobie.implicits._
import fr.thomasdufour.autodiff.extra.scalatest.AutodiffMatchers.~=

import loader.Loader
import model.Model

class ModelPersistenceSpec extends DatabaseSpec {

  import model.diff._

  val loadModel: IO[Model] = Loader.io.loadModel

  "storing and reading the model" must {
    "return it as-is" in {
      // setup
      val expected = loadModel.flatTap( WriteModel.writeModel( _, 1 ).transact( transactor ) ).unsafeRunSync()

      // exercise
      val actual = ReadModel.readModel( 1 ).transact( transactor ).unsafeRunSync()

      // verify
      actual must ~=( expected )
    }
  }

}
