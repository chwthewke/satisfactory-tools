package net.chwthewke.satisfactorytools
package data

import cats.effect.unsafe.implicits._
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import pureconfig.ConfigSource

import loader.Loader
import model.MapConfig

class LoadingSpec extends AnyWordSpec with Matchers {

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

      Inside.inside( Loader.io.loadResource[GameData].attempt.unsafeRunSync() ) { case Right( _ ) => succeed }
    }
  }

  "The model" should {
    "be loadable from resources" in {

      Inside.inside( Loader.io.loadModel.attempt.unsafeRunSync() ) {
        case Right( model ) =>
          model.items.size shouldBe <=( 256 )
          model.manufacturingRecipes.size shouldBe <=( 256 )
          model.extractionRecipes.size shouldBe <=( 256 )
      }

    }
  }

}
