package net.chwthewke.satisfactorytools
package persistence
package plans

import io.circe.Json

import protocol.PlanId

class PlanTreesChecks extends DatabaseSpec {

  "the statement" which {
    "selects plan trees" must {
      "type check" in {
        check( PlanTrees.statements.selectPlanTreeCommands( PlanId( 1 ) ) )
      }
    }

    "updates plan trees" must {
      "type check" in {
        check( PlanTrees.statements.updatePlanTreeCommands( PlanId( 1 ), Json.arr() ) )
      }
    }
  }

}
