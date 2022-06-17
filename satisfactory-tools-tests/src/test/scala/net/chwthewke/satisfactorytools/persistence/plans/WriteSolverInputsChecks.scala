package net.chwthewke.satisfactorytools
package persistence
package plans

import protocol.PlanId

class WriteSolverInputsChecks extends DatabaseSpec {
  "the statement" which {

    "upserts plan options" must {
      "type check" in {
        check( WriteSolverInputs.statements.upsertOptions )
      }
    }

    "upserts resource distributions" must {
      "type check" in {
        check( WriteSolverInputs.statements.upsertResourceDistribution )
      }
    }

    "deletes resource weights" must {
      "type check" in {
        check( WriteSolverInputs.statements.deleteResourceWeights )
      }
    }

    "inserts resource weights" must {
      "type check" in {
        check( WriteSolverInputs.statements.insertResourceWeights )
      }
    }

    "deletes bill items" must {
      "type check" in {
        check( WriteSolverInputs.statements.deleteBill )
      }
    }

    "inserts bill items" must {
      "type check" in {
        check( WriteSolverInputs.statements.insertBill )
      }
    }

    "deletes recipe list items" must {
      "type check" in {
        check( WriteSolverInputs.statements.deleteRecipeList )
      }
    }

    "inserts recipe list items" must {
      "type check" in {
        check( WriteSolverInputs.statements.insertRecipeList )
      }
    }

    "inserts the default recipe list" must {
      "type check" in {
        check( WriteSolverInputs.statements.insertDefaultRecipeList( PlanId( 1 ) ) )
      }
    }

    "adds all alternates to the recipe list" must {
      "type check" in {
        check( WriteSolverInputs.statements.insertAllAlternatesToRecipeList( PlanId( 1 ) ) )
      }
    }

    "removes all alternates from the recipe list" must {
      "type check" in {
        check( WriteSolverInputs.statements.deleteAllAlternatesFromRecipeList( PlanId( 1 ) ) )
      }
    }

  }
}
