package net.chwthewke.satisfactorytools
package protocol

import data.ClassName
import data.Countable
import data.Item
import model.GroupAssignments
import model.Machine
import model.Options
import prod.Factory
import prod.planning.FactoryPlan
import prod.tree.FactoryTree

sealed abstract class OutputTab extends Product { self =>
  type Data

  def typed: OutputTab.Aux[Data] =
    ( this: OutputTab { type Data = self.Data } ) match {
      case aux: OutputTab.Aux[d] => implicitly[d =:= Data].liftCo( aux )
    }
}

object OutputTab {
  sealed abstract class Aux[D] extends OutputTab { type Data = D }

  final case object Steps                                  extends Aux[( Factory, GroupAssignments )]
  final case object Items                                  extends Aux[Map[Item, ItemIO[ItemSrcDest.Global]]]
  final case object Machines                               extends Aux[Vector[Countable[Int, Machine]]]
  final case object Inputs                                 extends Aux[Vector[Countable[Double, Item]]]
  final case class CustomGroup( ix: Int )                  extends Aux[CustomGroupResult]
  final case object GroupIO                                extends Aux[Map[Item, ItemIO[ItemSrcDest.InterGroup]]]
  final case object Tree                                   extends Aux[FactoryTree]
  final case class PlanItemFlow( item: Option[ClassName] ) extends Aux[( FactoryPlan, Options, Option[Item] )]
}
