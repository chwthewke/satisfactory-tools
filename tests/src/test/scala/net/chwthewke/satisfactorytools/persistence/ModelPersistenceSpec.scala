package net.chwthewke.satisfactorytools
package persistence

import cats.effect.IO
import cats.syntax.flatMap._
import doobie.implicits._
import fr.thomasdufour.autodiff.extra.scalatest.AutodiffMatchers.~=

import loader.Loader
import model.Model

class ModelPersistenceSpec extends DatabaseSpec {

  import model.diff._

  val loadModel: IO[Model] = Loader.io.loadModel( DataVersionStorage.Update4 )

  def readingThePersistedModel( version: DataVersionStorage ): Unit =
    s"return it as-is (${version.entryName})" in {
      // setup
      val ( expected, versionId ) =
        Loader.io
          .loadModel( version )
          .mproduct( WriteModel.writeModel( _, version.modelVersion ).transact( transactor ) )
          .unsafeRunSync()

      // exercise
      val actual = ReadModel.readModel( versionId ).transact( transactor ).unsafeRunSync()

      // verify
      actual must ~=( expected )
    }

  "storing and reading the model" must {
    behave like readingThePersistedModel( DataVersionStorage.Update4 )
    behave like readingThePersistedModel( DataVersionStorage.Update5 )
    behave like readingThePersistedModel( DataVersionStorage.Update6 )
    behave like readingThePersistedModel( DataVersionStorage.Update7 )

  }

}
