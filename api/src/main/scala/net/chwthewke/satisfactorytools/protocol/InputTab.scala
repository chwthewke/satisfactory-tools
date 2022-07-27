package net.chwthewke.satisfactorytools
package protocol

import enumeratum.Enum
import enumeratum.EnumEntry

import model.Bill
import model.Options
import model.RecipeList
import model.ResourceOptions

sealed abstract class InputTab extends EnumEntry with Product {
  type Data
}

object InputTab extends Enum[InputTab] {
  type Aux[D] = InputTab { type Data = D }

  final case object Bill            extends InputTab { type Data = Bill            }
  final case object Recipes         extends InputTab { type Data = RecipeList      }
  final case object Options         extends InputTab { type Data = Options         }
  final case object ResourceOptions extends InputTab { type Data = ResourceOptions }

  override val values: IndexedSeq[InputTab] = findValues
}
