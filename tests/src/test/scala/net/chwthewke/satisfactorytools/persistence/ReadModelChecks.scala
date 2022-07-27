package net.chwthewke.satisfactorytools
package persistence

import protocol.ModelVersionId

class ReadModelChecks extends DatabaseSpec {
  "the select statement" which {
    "selects items" must {
      "type check" in {
        check( ReadModel.statements.selectItems( ModelVersionId( 1 ) ) )
      }
    }

    "selects machines" must {
      "type check" in {
        check( ReadModel.statements.selectMachines( ModelVersionId( 1 ) ) )
      }
    }

    "selects recipes" must {
      "type check" in {
        check( ReadModel.statements.selectRecipes( ModelVersionId( 1 ) ) )
      }
    }

    "selects recipe ingredients" must {
      "type check" in {
        check( ReadModel.statements.selectRecipeIngredients( ModelVersionId( 1 ) ) )
      }
    }

    "selects extraction recipes" must {
      "type check" in {
        check( ReadModel.statements.selectExtractionRecipes( ModelVersionId( 1 ) ) )
      }
    }

    "selects resource nodes" must {
      "type check" in {
        check( ReadModel.statements.selectResourceNodes( ModelVersionId( 1 ) ) )
      }
    }

    "selects item ids" must {
      "type check" in {
        check( ReadModel.statements.selectItemIds( ModelVersionId( 1 ) ) )
      }
    }

    "selects recipe ids" must {
      "type check" in {
        check( ReadModel.statements.selectRecipeIds( ModelVersionId( 1 ) ) )
      }
    }

    "reads all model versions" must {
      "type check" in {
        check( ReadModel.statements.selectModelVersions )
      }
    }

    "reads a model version" must {
      "type check" in {
        check( ReadModel.statements.selectModelVersion( ModelVersionId( 1 ) ) )
      }
    }
  }

}
