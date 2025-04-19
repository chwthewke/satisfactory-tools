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

  def forAllVersions( behaviour: DataVersionStorage => Unit ): Unit = {
    DataVersionStorage.values.foreach { version =>
      s"loading ${version.modelVersion.name}" should {
        behaviour( version )
      }
    }
  }

  "The GameData" when {

    forAllVersions {
      behave like gameDataProperties( _ )
    }

    def gameDataProperties( storage: DataVersionStorage ): Unit = {
      var gameData: GameData = null // Ugh, but rather this than loading every test

      "be loadable from resources" in {
        Inside.inside( Loader.io.loadResource[GameData]( storage ).attempt.unsafeRunSync() ) {
          case Right( data ) =>
            gameData = data
            succeed
        }
      }

      "have class names less than 256 characters long" in {

        Inside.inside( gameData ) {
          case GameData(
                items,
                extractors,
                manufacturers,
                powerGenerators,
                recipes,
                schematics,
                conveyorBelts,
                pipelines,
                buildingDescriptors
              ) =>
            all( items.keys.map( _.name.length ) ) shouldBe <=( 256 )
            all( extractors.keys.map( _.name.length ) ) shouldBe <=( 256 )
            all( manufacturers.keys.map( _.name.length ) ) shouldBe <=( 256 )
            all( recipes.map( _.className.name.length ) ) shouldBe <=( 256 )
            all( powerGenerators.keys.map( _.name.length ) ) shouldBe <=( 256 )
            all( schematics.map( _.className.name.length ) ) shouldBe <=( 256 )
            all( conveyorBelts.map( _.className.name.length ) ) shouldBe <=( 256 )
            all( pipelines.map( _.className.name.length ) ) shouldBe <=( 256 )
            all( buildingDescriptors.keys.map( _.name.length ) ) shouldBe <=( 256 )
        }

      }

      "have icon data for all extractors" in {
        Inside.inside( gameData ) {
          case GameData( _, extractors, _, _, _, _, _, _, _ ) =>
            all( extractors.keys.map( cn => gameData.getBuildingIcon( cn ) ) ) should not be empty
        }
      }

      "have icon data for all manufacturers" in {
        Inside.inside( gameData ) {
          case GameData( _, _, manufacturers, _, _, _, _, _, _ ) =>
            all( manufacturers.keys.map( cn => gameData.getBuildingIcon( cn ) ) ) should not be empty
        }
      }

      "have icon data for all power generators" in {
        Inside.inside( gameData ) {
          case GameData( _, _, _, powerGenerators, _, _, _, _, _ ) =>
            all( powerGenerators.keys.map( cn => gameData.getBuildingIcon( cn ) ) ) should not be empty
        }
      }

      "have icon data for all conveyor belts" in {
        Inside.inside( gameData ) {
          case GameData( _, _, _, _, _, _, conveyorBelts, _, _ ) =>
            all( conveyorBelts.map( cb => gameData.getBuildingIcon( cb.className ) ) ) should not be empty
        }
      }

      "have icon data for all pipelines" in {
        Inside.inside( gameData ) {
          case GameData( _, _, _, _, _, _, _, pipelines, _ ) =>
            all( pipelines.map( cb => gameData.getBuildingIcon( cb.className ) ) ) should not be empty
        }
      }
    }
  }

  "The model" when {
    forAllVersions {
      behave like modelProperties( _ )
    }

    def modelProperties( storage: DataVersionStorage ): Unit =
      "be loadable from resources" in {

        Inside.inside( Loader.io.loadModel( storage ).attempt.unsafeRunSync() ) {
          case Right( _ ) =>
            succeed
// left-over from when we had a scodec protocol
//            model.items.size shouldBe <=( 256 )
//            model.manufacturingRecipes.size shouldBe <=( 256 )
//            model.extractionRecipes.size shouldBe <=( 256 )
        }

      }
  }
}
