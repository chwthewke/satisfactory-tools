package net.chwthewke.satisfactorytools
package persistence

class WriteModelChecks extends DatabaseSpec {
  "the insert statement" which {
    "inserts an Item" must {
      "type check" in {
        check( WriteModel.statements.insertItem( 0 ) )
      }
    }

    "inserts a machine" must {
      "type check" in {
        check( WriteModel.statements.insertMachine( 0 ) )
      }
    }

    "inserts a recipe" must {
      "type check" in {
        check( WriteModel.statements.insertRecipe( 0 ) )
      }
    }

    "inserts a recipe ingredient" must {
      "type check" in {
        check( WriteModel.statements.insertRecipeIngredient )
      }
    }

    "inserts a recipe product" must {
      "type check" in {
        check( WriteModel.statements.insertRecipeProduct )
      }
    }

    "inserts an extraction recipe" must {
      "type check" in {
        check( WriteModel.statements.insertExtractionRecipe )
      }
    }

    "inserts a resource node" must {
      "type check" in {
        check( WriteModel.statements.insertResourceNodes( 0 ) )
      }
    }
  }

}
