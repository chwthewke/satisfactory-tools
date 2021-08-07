package net.chwthewke.satisfactorytools
package persistence
package plans

class WriteSolutionChecks extends DatabaseSpec {
  "the statement" which {

    "deletes a solution" must {
      "type check" in {
        check( WriteSolution.statements.deleteSolution )
      }
    }

    "inserts a solution id" must {
      "type check" in {
        check( WriteSolution.statements.insertSolution )
      }
    }

    "inserts a solution id with error" must {
      "type check" in {
        check( WriteSolution.statements.insertSolutionError )
      }
    }

    "inserts solution extraction recipes" must {
      "type check" in {
        check( WriteSolution.statements.insertSolutionExtractionRecipe )
      }
    }

    "inserts solution manufacturing recipes" must {
      "type check" in {
        check( WriteSolution.statements.insertSolutionManufacturingRecipe )
      }
    }

    "inserts solution extra inputs" must {
      "type check" in {
        check( WriteSolution.statements.insertSolutionExtraInput )
      }
    }

    "inserts solution extra outputs" must {
      "type check" in {
        check( WriteSolution.statements.insertSolutionExtraOutput )
      }
    }

  }
}
