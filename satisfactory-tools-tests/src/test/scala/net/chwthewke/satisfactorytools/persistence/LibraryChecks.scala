package net.chwthewke.satisfactorytools
package persistence

class LibraryChecks extends DatabaseSpec {

  "the statement" which {
    "inserts a named plan" must {
      "type check" in {
        check( Library.statements.insertPlan )
      }
    }

    "selects plan headers (unlimited)" must {
      "type check" in {
        check( Library.statements.selectPlanHeaders )
      }
    }

    "selects plan headers (limited)" must {
      "type check" in {
        check( Library.statements.selectPlanHeadersPage )
      }
    }

    "copies a solution header" must {
      "type check" in {
        check( Library.statements.copyPlanSolution )
      }
    }

    "copies a solution's extraction recipes" must {
      "type check" in {
        check( Library.statements.copySolutionExtractionRecipes )
      }
    }

    "copies a solution's manufacturing recipes" must {
      "type check" in {
        check( Library.statements.copySolutionManufacturingRecipes )
      }
    }

    "copies a solution's extra inputs" must {
      "type check" in {
        check( Library.statements.copySolutionExtraInputs )
      }
    }

    "copies a solution's extra outputs" must {
      "type check" in {
        check( Library.statements.copySolutionExtraOutputs )
      }
    }

    "selects a plan by user and name" must {
      "type check" in {
        check( Library.statements.selectPlanByName )
      }
    }

    "updates a plan's title" must {
      "type check" in {
        check( Library.statements.updatePlanName )
      }
    }

    "deletes a plan" must {
      "type check" in {
        check( Library.statements.deletePlan )
      }
    }
  }

}
