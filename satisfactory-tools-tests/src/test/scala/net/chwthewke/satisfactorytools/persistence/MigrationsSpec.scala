package net.chwthewke.satisfactorytools
package persistence

import doobie.implicits._
import scala.annotation.nowarn

@nowarn( "cat=lint-byname-implicit" )
class MigrationsSpec extends DatabaseSpec {

  "Initialising the database" should {
    "create the items table" in {
      check(
        sql"""SELECT "id" FROM "items" """.query[ItemId]
      )
    }

    "create the machines table" in {
      check(
        sql"""SELECT "id" from "machines" """.query[MachineId]
      )
    }

  }

}
