package net.chwthewke.satisfactorytools
package persistence

import protocol.PlanId

class PlansChecks extends DatabaseSpec {

  "the statement" which {

    "inserts an unnamed plan" must {
      "type check" in {
        check( Plans.statements.insertUnnamedPlan )
      }
    }

    "upserts plan options" must {
      "type check" in {
        check( Plans.statements.upsertOptions )
      }
    }

    "upserts resource distributions" must {
      "type check" in {
        check( Plans.statements.upsertResourceDistribution )
      }
    }

    "deletes resource weights" must {
      "type check" in {
        check( Plans.statements.deleteResourceWeights )
      }
    }

    "inserts resource weights" must {
      "type check" in {
        check( Plans.statements.insertResourceWeights )
      }
    }

    "deletes bill items" must {
      "type check" in {
        check( Plans.statements.deleteBill )
      }
    }

    "inserts bill items" must {
      "type check" in {
        check( Plans.statements.insertBill )
      }
    }

    "deletes recipe list items" must {
      "type check" in {
        check( Plans.statements.deleteRecipeList )
      }
    }

    "inserts recipe list items" must {
      "type check" in {
        check( Plans.statements.insertRecipeList )
      }
    }

    "deletes a solution" must {
      "type check" in {
        check( Plans.statements.deleteSolution )
      }
    }

    "inserts a solution id" must {
      "type check" in {
        check( Plans.statements.insertSolution )
      }
    }

    "inserts a solution id with error" must {
      "type check" in {
        check( Plans.statements.insertSolutionError )
      }
    }

    "inserts solution extraction recipes" must {
      "type check" in {
        check( Plans.statements.insertSolutionExtractionRecipe )
      }
    }

    "inserts solution manufacturing recipes" must {
      "type check" in {
        check( Plans.statements.insertSolutionManufacturingRecipe )
      }
    }

    "inserts solution extra inputs" must {
      "type check" in {
        check( Plans.statements.insertSolutionExtraInput )
      }
    }

    "inserts solution extra outputs" must {
      "type check" in {
        check( Plans.statements.insertSolutionExtraOutput )
      }
    }

    "selects a plan header" must {
      "type check" in {
        check( Plans.statements.selectPlanHeader )
      }
    }

    "selects bill items" must {
      "type check" in {
        check( Plans.statements.selectBillItems )
      }
    }

    "selects recipe list items" must {
      "type check" in {
        check( Plans.statements.selectRecipeListItems )
      }
    }

    "selects plan options" must {
      "type check" in {
        check( Plans.statements.selectOptions )
      }
    }

    "selects plan resource node options" must {
      "type check" in {
        check( Plans.statements.selectResourceNodes )
      }
    }

    "selects plan resource weights" must {
      "type check" in {
        check( Plans.statements.selectResourceWeights )
      }
    }

    "selects manufacturing recipes in group" must {
      "type check" in {
        check( Plans.statements.selectGroupManufacturingRecipes )
      }
    }

    "selects manufacturing recipes" must {
      "type check" in {
        check( Plans.statements.selectManufacturingRecipes )
      }
    }

    "selects manufacturing recipe groups" must {
      "type check" in {
        check( Plans.statements.selectRecipeGroups )
      }
    }

    "selects ungrouped manufacturing recipes" must {
      "type check" in {
        check( Plans.statements.selectUngroupedManufacturingRecipes )
      }
    }

    "selects extraction recipes" must {
      "type check" in {
        check( Plans.statements.selectExtractionRecipes )
      }
    }

    "selects extra inputs" must {
      "type check" in {
        check( Plans.statements.selectExtraInputs )
      }
    }

    "selects extra outputs" must {
      "type check" in {
        check( Plans.statements.selectExtraOutputs )
      }
    }

    "updates group count" must {
      "type check" in {
        check( Plans.statements.updateGroupCount )
      }
    }

    "updates group order" must {
      "type check" in {
        check( Plans.statements.updateGroupOrder( PlanId( 1 ), 1, 0 ) )
      }
    }

  }
}
