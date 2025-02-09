package net.chwthewke.satisfactorytools
package persistence

import cats.data.NonEmptyVector

import model.ExtractorType
import model.ModelVersion
import model.ResourcePurity
import protocol.ItemId
import protocol.ModelVersionId
import protocol.RecipeId

class WriteModelChecks extends DatabaseSpec {
  import WriteModel.RecipeIngredientId
  import WriteModel.RecipeProductId
  import WriteModel.statements._

  "the insert statement" which {
    "inserts an Item" must {
      "type check" in {
        check( insertItem( ModelVersionId( 1 ) ) )
      }
    }

    "inserts a machine" must {
      "type check" in {
        check( insertMachine( ModelVersionId( 1 ) ) )
      }
    }

    "inserts a recipe" must {
      "type check" in {
        check( insertRecipe( ModelVersionId( 1 ) ) )
      }
    }

    "inserts a recipe ingredient" must {
      "type check" in {
        check( insertRecipeIngredient )
      }
    }

    "inserts a recipe product" must {
      "type check" in {
        check( insertRecipeProduct )
      }
    }

    "inserts an extraction recipe" must {
      "type check" in {
        check( insertExtractionRecipe )
      }
    }

    "inserts a resource node" must {
      "type check" in {
        check( insertResourceNodes( ModelVersionId( 1 ) ) )
      }
    }
  }

  "the select statements" which {
    "inserts or updates a model version id" must {
      "type check" in {
        check( upsertModelVersion( ModelVersion( 1, "test" ) ) )
      }
    }
  }

  "the delete statement" which {
    "removes unused recipe ingredients" when {
      "there are some used ingredients" must {
        "type check" in {
          check(
            deleteUnusedRecipeIngredients(
              ModelVersionId( 1 ),
              Some( NonEmptyVector.of( RecipeIngredientId( 1 ), RecipeIngredientId( 2 ) ) )
            )
          )
        }
      }

      "there are no used ingredients" must {
        "type check" in {
          check(
            deleteUnusedRecipeIngredients( ModelVersionId( 1 ), None )
          )
        }
      }
    }

    "removes unused recipe products" when {
      "there are some used products" must {
        "type check" in {
          check(
            deleteUnusedRecipeProducts(
              ModelVersionId( 1 ),
              Some( NonEmptyVector.of( RecipeProductId( 1 ), RecipeProductId( 2 ) ) )
            )
          )
        }
      }

      "there are no used products" must {
        "type check" in {
          check(
            deleteUnusedRecipeProducts( ModelVersionId( 1 ), None )
          )
        }
      }
    }

    "removes unused extraction recipes" when {
      "there are some used extraction recipes" must {
        "type check" in {
          check(
            deleteUnusedExtractionRecipes(
              ModelVersionId( 1 ),
              Some(
                NonEmptyVector.of(
                  ( ItemId( 1 ), ResourcePurity.Pure: ResourcePurity, RecipeId( 1 ) ),
                  ( ItemId( 2 ), ResourcePurity.Normal: ResourcePurity, RecipeId( 2 ) )
                )
              )
            )
          )
        }
      }

      "there are no used extraction recipes" must {
        "type check" in {
          check(
            deleteUnusedExtractionRecipes( ModelVersionId( 1 ), None )
          )
        }
      }
    }

    "removes unused resource nodes" when {
      "there are some used resource nodes" must {
        "type check" in {
          check(
            deleteUnusedResourceNodes(
              ModelVersionId( 1 ),
              Some(
                NonEmptyVector.of(
                  ( ExtractorType.Miner: ExtractorType, ItemId( 1 ) ),
                  ( ExtractorType.OilPump: ExtractorType, ItemId( 2 ) )
                )
              )
            )
          )
        }
      }

      "there are no used resource nodes" must {
        "type check" in {
          check(
            deleteUnusedResourceNodes( ModelVersionId( 1 ), None )
          )
        }
      }
    }

    "removes unused recipes" when {
      "there are some used recipes" must {
        "type check" in {
          check( deleteUnusedRecipes( ModelVersionId( 1 ), Some( NonEmptyVector.of( RecipeId( 1 ), RecipeId( 2 ) ) ) ) )
        }
      }

      "there are no used recipes" must {
        "type check" in {
          check( deleteUnusedRecipes( ModelVersionId( 1 ), None ) )
        }
      }
    }

    "removes unused machines" when {
      "there are some used machines" must {
        "type check" in {
          check(
            deleteUnusedMachines( ModelVersionId( 1 ), Some( NonEmptyVector.of( MachineId( 1 ), MachineId( 2 ) ) ) )
          )
        }
      }

      "there are no used machines" must {
        "type check" in {
          check( deleteUnusedMachines( ModelVersionId( 1 ), None ) )
        }
      }
    }

    "removes unused items" when {
      "there are some used items" must {
        "type check" in {
          check(
            deleteUnusedItems( ModelVersionId( 1 ), Some( NonEmptyVector.of( ItemId( 1 ), ItemId( 2 ) ) ) )
          )
        }
      }

      "there are no used items" must {
        "type check" in {
          check( deleteUnusedItems( ModelVersionId( 1 ), None ) )
        }
      }
    }

  }

}
