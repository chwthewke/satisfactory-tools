package net.chwthewke.satisfactorytools
package protocol

import data.Countable
import data.Item
import model.GroupAssignments
import model.Machine
import prod.Factory
import prod.adv.tree.FactoryTree

sealed abstract class OutputTab extends Product {
  type Data
}

object OutputTab {
  type Aux[D] = OutputTab { type Data = D }

  final case object Steps                 extends OutputTab { type Data = ( Factory, GroupAssignments )             }
  final case object Items                 extends OutputTab { type Data = Map[Item, ItemIO[ItemSrcDest.Global]]     }
  final case object Machines              extends OutputTab { type Data = Vector[Countable[Int, Machine]]           }
  final case object Inputs                extends OutputTab { type Data = Vector[Countable[Double, Item]]           }
  final case class CustomGroup( ix: Int ) extends OutputTab { type Data = CustomGroupResult                         }
  final case object GroupIO               extends OutputTab { type Data = Map[Item, ItemIO[ItemSrcDest.InterGroup]] }
  final case object Tree                  extends OutputTab { type Data = FactoryTree                               }
}
