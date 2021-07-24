package net.chwthewke.satisfactorytools
package persistence

class ReadModelChecks extends DatabaseSpec {
  "the select statement" which {
    "selects items" must {
      "type check" in {
        check( ReadModel.statements.selectItems( 0 ) )
      }
    }

    "selects machines" must {
      "type check" in {
        check( ReadModel.statements.selectMachines( 0 ) )
      }
    }

    "selects recipes" must {
      "type check" in {
        check( ReadModel.statements.selectRecipes( 0 ) )
      }
    }

    "selects recipe ingredients" must {
      "type check" in {
        check( ReadModel.statements.selectRecipeIngredients( 0 ) )
      }
    }

    "selects extraction recipes" must {
      "type check" in {
        check( ReadModel.statements.selectExtractionRecipes( 0 ) )
      }
    }

    "selects resource nodes" must {
      "type check" in {
        check( ReadModel.statements.selectResourceNodes( 0 ) )
      }
    }
  }

}
