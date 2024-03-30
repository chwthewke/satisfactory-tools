package net.chwthewke.satisfactorytools
package protocol

import enumeratum.Enum
import enumeratum.EnumEntry

import model.Bill
import model.Options
import model.RecipeList
import model.ResourceOptions

sealed abstract class InputTab extends EnumEntry with Product { self =>
  type Data

  def typed: InputTab.Aux[Data] =
    ( this: InputTab { type Data = self.Data } ) match {
      case aux: InputTab.Aux[d] => implicitly[d =:= Data].liftCo( aux )
    }
}

object InputTab extends Enum[InputTab] {
  sealed abstract class Aux[D] extends InputTab { type Data = D }

  final case object Bill            extends Aux[Bill]
  final case object Recipes         extends Aux[RecipeList]
  final case object Options         extends Aux[Options]
  final case object ResourceOptions extends Aux[ResourceOptions]

  override val values: IndexedSeq[InputTab] = findValues
}
