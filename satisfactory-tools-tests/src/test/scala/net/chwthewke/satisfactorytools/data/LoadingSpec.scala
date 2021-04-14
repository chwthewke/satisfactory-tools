package net.chwthewke.satisfactorytools
package data

import cats.effect.Blocker
import cats.effect.ContextShift
import cats.effect.IO
import cats.syntax.applicativeError._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.foldable._
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax._
import scala.concurrent.ExecutionContext

class LoadingSpec extends AnyWordSpec with Matchers {

  "The prod configuration" should {
    "be loadable from reference.conf" in {
      Inside.inside( ConfigSource.defaultReference.load[ProductionConfig] ) {
        case Right( config ) =>
          succeed
      }
    }
  }

  "The map configuration" should {
    "be loadable from resources" in {
      Inside.inside( ConfigSource.resources( "map.conf" ).load[MapConfig] ) {
        case Right( map ) =>
          succeed
      }
    }
  }

  "The GameData" should {
    "be loadable from resources" in {

      implicit val ioCS: ContextShift[IO] = IO.contextShift( ExecutionContext.global )

      Inside.inside( Blocker[IO].use( Loader.io.loadResource[GameData] ).attempt.unsafeRunSync() ) {
        case Right( data ) =>
          succeed
      }
    }
  }

  "The model" should {
    "be loadable from resources" in {

      implicit val ioCS: ContextShift[IO] = IO.contextShift( ExecutionContext.global )

      Inside.inside(
        Blocker[IO]
          .use(
            b =>
              ( Loader.io.loadResource[GameData]( b ), ConfigSource.resources( "map.conf" ).loadF[IO, MapConfig]( b ) )
                .mapN( ( data, map ) => data.toModel( map ) )
          )
          .attemptT
          .subflatMap( _.toEither.leftMap( errs => new IllegalArgumentException( errs.intercalate( "\n" ) ) ) )
          .value
          .unsafeRunSync()
      ) {
        case Right( model ) =>
          succeed
      }

    }
  }

}
