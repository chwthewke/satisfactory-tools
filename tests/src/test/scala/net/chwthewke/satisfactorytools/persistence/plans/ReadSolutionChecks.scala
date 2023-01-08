package net.chwthewke.satisfactorytools
package persistence
package plans

class ReadSolutionChecks extends DatabaseSpec {
  "the statement" which {

    "selects manufacturing recipes" must {
      "type check" in {
        check( ReadSolution.statements.selectManufacturingRecipes )
      }
    }

    "selects extraction recipes" must {
      "type check" in {
        check( ReadSolution.statements.selectExtractionRecipes )
      }
    }

    "selects extra inputs" must {
      "type check" in {
        check( ReadSolution.statements.selectExtraInputs )
      }
    }

    "selects extra outputs" must {
      "type check" in {
        check( ReadSolution.statements.selectExtraOutputs )
      }
    }

  }
}
