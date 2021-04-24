package net.chwthewke.satisfactorytools
package data

import cats.effect.ContextShift
import cats.effect.IO
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import pureconfig.ConfigSource
import scala.concurrent.ExecutionContext

class LoadingSpec extends AnyWordSpec with Matchers {
  implicit val ioCS: ContextShift[IO] = IO.contextShift( ExecutionContext.global )

  "The prod configuration" should {
    "be loadable from reference.conf" in {
      Inside.inside( ConfigSource.defaultReference.load[ProductionConfig] ) { case Right( _ ) => succeed }
    }
  }

  "The map configuration" should {
    "be loadable from resources" in {
      Inside.inside( ConfigSource.resources( "map.conf" ).load[MapConfig] ) { case Right( _ ) => succeed }
    }
  }

  "The GameData" should {
    "be loadable from resources" in {

      Inside.inside( Loader.io.use( _.loadResource[GameData] ).attempt.unsafeRunSync() ) { case Right( _ ) => succeed }
    }
  }

  "The model" should {
    "be loadable from resources" in {

      Inside.inside(
        Loader[IO]
          .use( loader => loader.loadModel )
          .attempt
          .unsafeRunSync()
      ) { case Right( _ ) => succeed }

    }
  }

  "The whole solver inputs" should {
    "be loadable from resources" in {
      Inside.inside(
        Loader[IO]
          .use( loader => loader.loadModel.flatMap( loader.loadSolverInputs( _, ConfigSource.defaultReference ) ) )
          .attempt
          .unsafeRunSync()
      ) { case Right( _ ) => succeed }

    }
  }

}
