package net.chwthewke.satisfactorytools
package protocol

import scodec.Codec
import scodec.bits.Bases
import scodec.bits.BitVector

import data.ClassName
import data.Countable
import data.Item
import model.GroupAssignments
import model.Machine
import model.Recipe
import net.chwthewke.satisfactorytools.model.Bill
import prod.Factory
import prod.tree.FactoryTree

sealed abstract class OutputTab extends Product {
  type Data
  def editMode: Boolean = false
}

object OutputTab {
  type Aux[D] = OutputTab { type Data = D }

  final case class Steps( editGroups: Boolean ) extends OutputTab {
    type Data = ( Factory, GroupAssignments[ClassName] )
    override def editMode: Boolean = editGroups
  }
  final case object Items    extends OutputTab { type Data = Map[Item, ItemIO[ItemSrcDest.Global]] }
  final case object Machines extends OutputTab { type Data = Vector[Countable[Int, Machine]]       }
  final case object Inputs   extends OutputTab { type Data = Vector[Countable[Double, Item]]       }
  final case class CustomGroup( ix: Int, editOrder: Boolean ) extends OutputTab {
    type Data = CustomGroupResult
    override def editMode: Boolean = editOrder
  }
  final case object GroupIO extends OutputTab { type Data = Map[Item, ItemIO[ItemSrcDest.InterGroup]] }
  final case object Tree    extends OutputTab { type Data = FactoryTree                               }
  final case class ItemFlow( state: ItemFlow.State ) extends OutputTab { type Data = ItemFlow.Data }

  object ItemFlow {

    case class Data(
        bill: Bill,
        factory: Factory,
        groupAssignments: GroupAssignments[ClassName],
        recipeIds: Vector[( RecipeId, Recipe )],
        itemIds: Vector[( ItemId, Item )]
    ) {
      lazy val itemsById: Map[ItemId, Item] = itemIds.toMap
      lazy val itemIdsByClass: Map[ClassName, ItemId] = itemIds.map {
        case ( id, item ) => ( item.className, id )
      }.toMap
    }

    case class State( currentItem: Option[ItemId] ) {
      def repr: Option[String] =
        State.stateCodec.encode( this ).toOption.map( _.toBase64UrlNoPad )
    }

    object State {
      val default: State = State( None )

      def parse( str: String ): Option[State] =
        BitVector.fromBase64( str, Bases.Alphabets.Base64UrlNoPad ).flatMap( stateCodec.decodeValue( _ ).toOption )

      implicit val stateCodec: Codec[State] = {
        import scodec.codecs._

        val currentItemCodec: Codec[Option[ItemId]] =
          int32.xmap(
            n => Option.when( n >= 0 )( ItemId( n ) ),
            idOpt => idOpt.fold( -1 )( _.id )
          )

        currentItemCodec.xmap( State( _ ), _.currentItem )
      }
    }

  }
}
