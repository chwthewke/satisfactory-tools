package net.chwthewke.satisfactorytools
package data

import cats.effect.unsafe.implicits._
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import pureconfig.ConfigSource

import loader.Loader

class LoadingSpec extends AnyWordSpec with Matchers {

  "The prod configuration" should {
    "be loadable from reference.conf" in {
      Inside.inside( ConfigSource.defaultReference.load[ProductionConfig] ) { case Right( _ ) => succeed }
    }
  }

  "The map configuration" should {
    "be loadable from resources" in {
      Inside.inside( Loader.io.loadMapConfigs.attempt.unsafeRunSync() ) { case Right( _ ) => succeed }
    }
  }

  "The GameData" when {

    "loading Update 4 data" should {
      behave like gameDataProperties( DataVersionStorage.Update4 )
    }

    "loading Update 5 data" should {
      behave like gameDataProperties( DataVersionStorage.Update5 )
    }

    "loading Update 6 data" should {
      behave like gameDataProperties( DataVersionStorage.Update6 )
    }

    "loading Update 7 data" should {
      behave like gameDataProperties( DataVersionStorage.Update7 )
    }

    def gameDataProperties( storage: DataVersionStorage ): Unit = {
      "be loadable from resources" in {
        Inside.inside( Loader.io.loadResource[GameData]( storage ).attempt.unsafeRunSync() ) {
          case Right( _ ) => succeed
        }
      }

      "have class names less than 256 characters long" in {

        Inside.inside( Loader.io.loadResource[GameData]( storage ).unsafeRunSync() ) {
          case GameData( items, extractors, manufacturers, recipes, nuclearGenerators, schematics ) =>
            all( items.keys.map( _.name.length ) ) shouldBe <=( 256 )
            all( extractors.keys.map( _.name.length ) ) shouldBe <=( 256 )
            all( manufacturers.keys.map( _.name.length ) ) shouldBe <=( 256 )
            all( recipes.map( _.className.name.length ) ) shouldBe <=( 256 )
            all( nuclearGenerators.keys.map( _.name.length ) ) shouldBe <=( 256 )
            all( schematics.map( _.className.name.length ) ) shouldBe <=( 256 )
        }

      }
    }
  }

  "The model" when {
    "loading Update 4 data" should {
      behave like modelProperties( DataVersionStorage.Update4 )
    }

    "loading Update 5 data" should {
      behave like modelProperties( DataVersionStorage.Update5 )
    }

    "loading Update 6 data" should {
      behave like modelProperties( DataVersionStorage.Update6 )
    }

    "loading Update 7 data" should {
      behave like modelProperties( DataVersionStorage.Update7 )
    }

    def modelProperties( storage: DataVersionStorage ): Unit =
      "be loadable from resources" in {

        Inside.inside( Loader.io.loadModel( storage ).attempt.unsafeRunSync() ) {
          case Right( model ) =>
            model.items.size shouldBe <=( 256 )
            model.manufacturingRecipes.size shouldBe <=( 256 )
            model.extractionRecipes.size shouldBe <=( 256 )
        }

      }
  }

}
