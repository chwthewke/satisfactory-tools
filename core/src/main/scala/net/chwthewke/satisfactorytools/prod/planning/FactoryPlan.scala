package net.chwthewke.satisfactorytools
package prod
package planning

import cats.Order.catsKernelOrderingForOrder
import cats.data.Ior
import cats.data.NonEmptyMap
import cats.data.NonEmptyVector
import cats.data.State
import cats.syntax.align._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.traverse._
import cats.syntax.vector._
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

import data.Countable
import data.Item
import model.Bill

// TODO given a consumer split, can we express the difference
//  between "split the producers" and "split the producers' output"?
case class FactoryPlan(
    processes: SortedMap[ProcessId, Process],
    itemFlows: SortedMap[Item, FactoryPlan.ItemFlow]
) {

  import FactoryPlan._

  def splitProducersOf( item: Item ): State[Env, FactoryPlan] = {
    val _ = Env.pure(
      itemFlows
        .get( item )
        .map {
          case ItemFlow( consumers, producers, priorities ) =>
            ???
        }
    )

    ???
  }

  def unsplitProducersOf( item: Item ): FactoryPlan = ???
}

object FactoryPlan {
  case class Env( nextId: ProcessId.Synthetic, balancer: FlowBalancer ) {
    private def incrId: Env = copy( nextId = nextId.copy( id = nextId.id + 1 ) )
  }

  object Env {
    def withBalancer[A]( k: FlowBalancer => A ): State[Env, A] =
      State.get[Env].map( env => k( env.balancer ) )

    def getNextId: State[Env, ProcessId.Synthetic] =
      State( env => ( env.incrId, env.nextId ) )

    def pure[A]( a: A ): State[Env, A] = State.pure( a )
  }

  case class ConsumerGroup( processes: NonEmptyVector[ProcessId] )
  object ConsumerGroup {}

  /**
   * @param consumers Groups of processes (full block or part)
   * @param producers Map of recipe block to processes (partial blocks, or just the whole block if None)
   * @param priorities Preferred assignments of producers to consumers
   */
  case class ItemFlow(
      consumers: NonEmptyVector[ConsumerGroup],
      producers: NonEmptyMap[ProcessId.Natural, Option[Vector[ProcessId.Synthetic]]],
      priorities: Vector[NonEmptyVector[( Int, Int )]] // indexes into (producers.keySet x consumers)
  ) {
    def transports(balancer: FlowBalancer): NonEmptyVector[Transport[ProcessId]] =
      ???
//    def moveConsumer( id: ProcessId, to: Int ): ItemFlow =
//      consumers.zipWithIndex
//        .traverse {
//          case ( group, `to` ) =>
//            ConsumerGroup( NonEmptyVector.one( id ).prependVector( group.processes.filterNot( _ == id ) ) ).some
//          case ( group, _ ) =>
//            group.processes.filterNot( _ == id ).toNev.map( ConsumerGroup( _ ) )
//        }
//        .map(
//          Option
//            .when( to > consumers.length )( ConsumerGroup( NonEmptyVector.one( id ) ) )
//            .foldLeft( _ )( _.append( _ ) )
//        )
//        .fold( this )( ItemFlow( _, producers ) )

//    def splitProducer

    def unassign( producer: ProcessId.Natural, consumerGroupIndex: Int ): ItemFlow = ???

    def assign( producer: ProcessId.Natural, consumerGroupIndex: Int ): ItemFlow = {
      ???
    }

  }

  object ItemFlow {
    case class Env( processes: SortedMap[ProcessId, Process], planEnv: FactoryPlan.Env )

    object Env {
      def removeProcesses( ids: Iterable[ProcessId] ): State[Env, Unit] =
        State.modify( env => env.copy( processes = env.processes.removedAll( ids ) ) )

      def register( process: Process ): State[Env, ProcessId] = {
        lift( FactoryPlan.Env.getNextId )
          .flatTap( id => State.modify( env => env.copy( processes = env.processes.updated( id, process ) ) ) )
          .widen
      }

      def withBalancer[A]( k: FlowBalancer => A ): State[Env, A] =
        lift( FactoryPlan.Env.withBalancer( k ) )

      private def lift[A]( fa: State[FactoryPlan.Env, A] ): State[Env, A] =
        fa.transformS( _.planEnv, ( env, _ ) => env )
    }
  }

//  abstract class ProducerTargets {
//    def assigned: SortedSet[Int]
//  }
//
//  case class SingleProducerTargets( assigned: SortedSet[Int] ) extends ProducerTargets
//  case class SplitProducerTargets( assigned: SortedSet[Int], subProcesses: SortedMap[Int, ProcessId] )
//      extends ProducerTargets
//
//  object ProducerTargets {
//    def unassigned: ProducerTargets = SingleProducerTargets( SortedSet.empty )
//  }

  def from( bill: Bill, factory: Factory ): FactoryPlan = {

    val processes: Vector[( ProcessId.Natural, Process )] =
      factory.extraction.map( r => ( ProcessId.recipe( r ), Process.Extraction( r ) ) ) ++
        factory.manufacturing.map( r => ( ProcessId.recipe( r ), Process.Manufacturing( r ) ) ) ++
        bill.items.map( i => ( ProcessId.request( i ), Process.Request( i ) ) ) ++
        factory.extraOutputs.map( i => ( ProcessId.byProduct( i ), Process.ByProduct( i ) ) )

    val itemsProduced: SortedMap[Item, NonEmptyVector[ProcessId.Natural]] =
      processes.foldMap {
        case ( id, process ) =>
          process.producedPerMinute.foldMap {
            case Countable( item, _ ) =>
              SortedMap( item -> NonEmptyVector.one( id ) )
          }
      }

    val itemsConsumed: SortedMap[Item, NonEmptyVector[ProcessId.Natural]] =
      processes.foldMap {
        case ( id, process ) =>
          process.consumedPerMinute.foldMap {
            case Countable( item, _ ) =>
              SortedMap( item -> NonEmptyVector.one( id ) )
          }
      }

    val itemFlows: SortedMap[Item, ItemFlow] =
      itemsProduced.align( itemsConsumed ).collect {
        case ( item, Ior.Both( producers, consumers ) ) =>
          (
            item,
            ItemFlow(
              NonEmptyVector.one( ConsumerGroup( consumers ) ),
              producers.tupleRight( none ).toNem,
              Vector.empty
            )
          )
      }

    FactoryPlan( processes.to( SortedMap ), itemFlows )
  }

}

//  def flowsOf( item: Item ): Vector[Transport[( ProcessId, Process )]] =
//    itemFlows
//      .get( item )
//      .flatMap( _.toVector.traverse( _.traverse( id => processes.get( id ).tupleLeft( id ) ) ) )
//      .orEmpty

//  def moveConsumer( forItem: Item, processId: ProcessId, to: Int ): FactoryPlan = {
//        itemFlows.updatedWith(forItem)(_.map(
//          transports =>
//            transports.zipWithIndex.map {
//              case (Transport(producers, consumers), ix) =>
//                Transport(producers, consumers.filterNot())
//            }
//        ))
//  }

//  def packIds: FactoryPlan = {
//    val (
//      packedProcesses: SortedMap[ProcessId, Process],
//      translation: SortedMap[ProcessId.Synthetic, ProcessId.Synthetic],
//      nextId: ProcessId.Synthetic
//    ) =
//      processes.foldLeft(
//        (
//          SortedMap.empty[ProcessId, Process],
//          SortedMap.empty[ProcessId.Synthetic, ProcessId.Synthetic],
//          ProcessId.Synthetic( 0 )
//        )
//      ) {
//        case ( ( processAcc, translateAcc, nextId ), ( id, process ) ) =>
//          id match {
//            case n: ProcessId.Synthetic =>
//              (
//                processAcc.updated( nextId, process ),
//                translateAcc.updated( n, nextId ),
//                nextId.copy( id = nextId.id + 1 )
//              )
//            case _ =>
//              (
//                processAcc.updated( id, process ),
//                translateAcc,
//                nextId
//              )
//          }
//      }
//
//    def translate( id: ProcessId ): ProcessId =
//      id match {
//        case n: ProcessId.Synthetic => translation( n )
//        case _                      => id
//      }
//
//    val packedItemFlows: SortedMap[Item, NonEmptyVector[Transport[ProcessId]]] =
//      itemFlows.fmap( _.map( _.map( translate ) ) )
//
//    FactoryPlan( packedProcesses, packedItemFlows )( nextId )
//  }
