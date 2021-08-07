package net.chwthewke.satisfactorytools
package persistence
package plans

import protocol.PlanId

class CustomGroupsChecks extends DatabaseSpec {
  "the statement" which {

    "selects manufacturing recipe groups" must {
      "type check" in {
        check( CustomGroups.statements.selectRecipeGroups )
      }
    }

    "selects ungrouped manufacturing recipes" must {
      "type check" in {
        check( CustomGroups.statements.selectUngroupedManufacturingRecipes )
      }
    }

    "updates group count" must {
      "type check" in {
        check( CustomGroups.statements.updateGroupCount )
      }
    }

    "updates group order" must {
      "type check" in {
        check( CustomGroups.statements.updateGroupOrder( PlanId( 1 ), 1, 0 ) )
      }
    }
  }

}
