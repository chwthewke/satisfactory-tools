package net.chwthewke.satisfactorytools
package protocol

import scodec.Codec
import scodec.bits.Bases
import scodec.bits.BitVector

import data.ClassName
import data.Countable
import data.Item
import model.Bill
import model.GroupAssignments
import model.Machine
import model.Recipe
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

    case class ItemState(
        producerRecipeOrder: Vector[RecipeId],
        consumerRecipeOrder: Vector[RecipeId]
    )

    case class State(
        currentItem: Option[ItemId],
        perItem: Map[ItemId, ItemState]
    ) {
      def repr: Option[String] =
        State.stateCodec.encode( this ).toOption.map( _.toBase64UrlNoPad )
    }

    object State {
      val default: State = State( None, Map.empty )

      def parse( str: String ): Option[State] =
        BitVector.fromBase64( str, Bases.Alphabets.Base64UrlNoPad ).flatMap( stateCodec.decodeValue( _ ).toOption )

      implicit val stateCodec: Codec[State] = {
        import scodec.codecs._

        val itemIdCodec: Codec[ItemId] = int32.xmap( ItemId( _ ), _.id )

        val currentItemCodec: Codec[Option[ItemId]] =
          int32.xmap(
            n => Option.when( n >= 0 )( ItemId( n ) ),
            idOpt => idOpt.fold( -1 )( _.id )
          )

        val recipeIdCodec: Codec[RecipeId]          = int32.xmap( RecipeId( _ ), _.id )
        val recipeIdsCodec: Codec[Vector[RecipeId]] = vectorOfN( int32, recipeIdCodec )
        val itemStateCodec: Codec[ItemState] =
          ( recipeIdsCodec ~ recipeIdsCodec )
            .xmap(
              liftF2ToNestedTupleF( ItemState( _, _ ) ),
              state => state.producerRecipeOrder ~ state.consumerRecipeOrder
            )

        val itemStateEntryCodec: Codec[( ItemId, ItemState )] = itemIdCodec ~ itemStateCodec
        val itemStatesCodec: Codec[Map[ItemId, ItemState]] =
          vectorOfN( int32, itemStateEntryCodec ).xmap( _.toMap, _.toVector )

        ( currentItemCodec ~ itemStatesCodec ).xmap( State( _, _ ), state => state.currentItem ~ state.perItem )
      }
    }

  }
}
