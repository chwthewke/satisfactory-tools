package net.chwthewke.satisfactorytools
package persistence
package plans

class ReadSolverInputsChecks extends DatabaseSpec {
  "the statement" which {

    "selects bill items" must {
      "type check" in {
        check( ReadSolverInputs.statements.selectBillItems )
      }
    }

    "selects recipe list items" must {
      "type check" in {
        check( ReadSolverInputs.statements.selectRecipeListItems )
      }
    }

    "selects plan options" must {
      "type check" in {
        check( ReadSolverInputs.statements.selectOptions )
      }
    }

    "selects plan resource node options" must {
      "type check" in {
        check( ReadSolverInputs.statements.selectResourceNodes )
      }
    }

    "selects plan resource weights" must {
      "type check" in {
        check( ReadSolverInputs.statements.selectResourceWeights )
      }
    }

  }
}
