package net.chwthewke.satisfactorytools
package persistence
package plans

import cats.data.NonEmptyVector

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

    "reads the group order" must {
      "type check" in {
        check( CustomGroups.statements.selectGroupOrder( PlanId( 1 ), 1, NonEmptyVector.of( 2, 3 ) ) )
      }
    }

    "updates group order" must {
      "type check" in {
        check( CustomGroups.statements.updateGroupOrder( 1 ) )
      }
    }

    "toggles section separator" must {
      "type check" in {
        check( CustomGroups.statements.toggleSectionBefore( PlanId( 1 ), 1, 2 ) )
      }
    }
  }

}
