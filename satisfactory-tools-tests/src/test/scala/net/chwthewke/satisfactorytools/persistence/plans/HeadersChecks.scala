package net.chwthewke.satisfactorytools
package persistence
package plans

class HeadersChecks extends DatabaseSpec {
  "the statement" which {

    "inserts an unnamed plan" must {
      "type check" in {
        check( Headers.statements.insertUnnamedPlan )
      }
    }

    "selects a plan header" must {
      "type check" in {
        check( Headers.statements.selectPlanHeader )
      }
    }
  }
}
