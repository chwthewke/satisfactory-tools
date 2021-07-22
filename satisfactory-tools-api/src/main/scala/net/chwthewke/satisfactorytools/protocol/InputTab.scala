package net.chwthewke.satisfactorytools
package protocol

import enumeratum.Enum
import enumeratum.EnumEntry

import model.Bill
import model.Options
import model.RecipeList
import model.ResourceOptions

sealed abstract class InputTab extends EnumEntry {
  type Data
}

object InputTab extends Enum[InputTab] {
  type Aux[D] = InputTab { type Data = D }

  final case object BillTab            extends InputTab { type Data = Bill            }
  final case object RecipeListTab      extends InputTab { type Data = RecipeList      }
  final case object OptionsTab         extends InputTab { type Data = Options         }
  final case object ResourceOptionsTab extends InputTab { type Data = ResourceOptions }

  override val values: IndexedSeq[InputTab] = findValues
}
