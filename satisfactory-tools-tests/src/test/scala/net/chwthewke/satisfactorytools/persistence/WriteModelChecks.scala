package net.chwthewke.satisfactorytools
package persistence

class WriteModelChecks extends DatabaseSpec {
  "the insert statement" which {
    "inserts an Item" must {
      "type check" in {
        check( WriteModel.insertItem( 0 ) )
      }
    }

    "inserts a machine" must {
      "type check" in {
        check( WriteModel.insertMachine( 0 ) )
      }
    }

    "inserts a recipe" must {
      "type check" in {
        check( WriteModel.insertRecipe( 0 ) )
      }
    }

    "inserts a recipe ingredient" must {
      "type check" in {
        check( WriteModel.insertRecipeIngredient )
      }
    }

    "inserts a recipe product" must {
      "type check" in {
        check( WriteModel.insertRecipeProduct )
      }
    }

    "inserts an extraction recipe" must {
      "type check" in {
        check( WriteModel.insertExtractionRecipe )
      }
    }

    "inserts a resource node" must {
      "type check" in {
        check( WriteModel.insertResourceNodes( 0 ) )
      }
    }
  }

}
