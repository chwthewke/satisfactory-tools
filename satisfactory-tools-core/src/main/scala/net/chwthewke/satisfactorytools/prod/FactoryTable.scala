package net.chwthewke.satisfactorytools
package prod

import mouse.boolean._
//

object FactoryTable {
  import Alignment.AlignLeft
  import Alignment.AlignRight

  val sep = " | "

  def columnsForBlock(
      recipeName: String,
      totalAmount: Double,
      amountPerUnit: Double,
      machineName: String,
      machineCount: Int,
      clockSpeed: Int,
      power: Double
  ): Vector[String] =
    Vector(
      f"$totalAmount%4.3f",
      sep,
      recipeName.stripPrefix( "Alternate: " ),
      " ",
      recipeName.startsWith( "Alternate: " ).fold( "ALT", "" ),
      sep,
      f"$machineCount%3d",
      " ",
      machineName,
      sep,
      f"$amountPerUnit%3.3f / unit @ $clockSpeed%-3d%%",
      sep,
      f"$power%4.2f",
      f" MW"
    )

  val headers =
    Vector(
      ( 5, "Recipe" ),
      ( 1, sep ),
      ( 3, "Machines" ),
      ( 1, sep ),
      ( 1, "Rate" ),
      ( 1, sep ),
      ( 2, "Power" )
    )

  val alignment =
    Vector(
      AlignRight,
      AlignLeft,
      AlignLeft,
      AlignLeft,
      AlignRight,
      AlignLeft,
      AlignRight,
      AlignLeft,
      AlignLeft,
      AlignLeft,
      AlignRight,
      AlignLeft,
      AlignRight,
      AlignLeft
    )

}
