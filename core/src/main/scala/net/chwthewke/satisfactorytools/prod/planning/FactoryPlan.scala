package net.chwthewke.satisfactorytools
package prod
package planning

import cats.Functor
import cats.Monad
import cats.MonadThrow
import cats.Order.catsKernelOrderingForOrder
import cats.data.Ior
import cats.data.NonEmptyMap
import cats.data.NonEmptyVector
import cats.data.State
import cats.data.StateT
import cats.syntax.align._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.vector._
import scala.collection.immutable.SortedMap

import data.Countable
import data.Item
import model.Bill

// TODO given a consumer split, can we express the difference
//  between "split the producers" and "split the producers' output"?
case class FactoryPlan(
    processes: SortedMap[ProcessId, Process],
    // below is the persisted part
    itemFlows: SortedMap[Item, FactoryPlan.ItemFlow]
) {

//  def splitProducersOf( item: Item ): State[Env, FactoryPlan] = {
//    val _ = Env.pure(
//      itemFlows
//        .get( item )
//        .map {
//          case ItemFlow( consumers, producers, priorities ) =>
//            ???
//        }
//    )
//
//    ???
//  }

  def unsplitProducersOf( item: Item ): FactoryPlan = ???
}

object FactoryPlan {
  case class Env[F[_]]( nextId: ProcessId.Synthetic, balancer: FlowBalancer[F] ) {
    private def incrId: Env[F] = copy( nextId = nextId.copy( id = nextId.id + 1 ) )
  }

  object Env {
    def withBalancer[F[_]: Monad, A]( k: FlowBalancer[F] => F[A] ): StateT[F, Env[F], A] =
      StateT.inspectF[F, Env[F], A]( env => k( env.balancer ) )

    def getNextId[F[_]]: State[Env[F], ProcessId.Synthetic] =
      State( env => ( env.incrId, env.nextId ) )

    def pure[F[_], A]( a: A ): State[Env[F], A] = State.pure( a )
  }

  case class ConsumerGroup( processes: NonEmptyVector[ProcessId] )
  object ConsumerGroup {}

  /**
   * @param consumers
   *   Groups of processes (full block or part)
   * @param producers
   *   Map of recipe block to processes (partial blocks, or just the whole block if None)
   * @param priorities
   *   Preferred assignments of producers to consumers
   */
  case class ItemFlow(
      consumers: NonEmptyVector[ConsumerGroup],
      producers: NonEmptyMap[ProcessId.Natural, Option[NonEmptyVector[ProcessId.Synthetic]]],
      priorities: Vector[NonEmptyVector[( Int, Int )]] // indexes into (producers.keySet x consumers)
  ) {

    def transports[F[_]: MonadThrow]: StateT[F, ItemFlow.Env[F], NonEmptyVector[Transport[ProcessId]]] =
      StateT.inspectF[F, ItemFlow.Env[F], NonEmptyVector[Transport[ProcessId]]] { env =>
        val producersV: Vector[( ProcessId.Natural, Option[NonEmptyVector[ProcessId.Synthetic]] )] =
          producers.toSortedMap.toVector

        env.planEnv.balancer
          .balance(
            producersV
              .map { case ( id, _ ) => env.processes.get( id ).foldMap( _.produced( env.item ) ) },
            consumers.toVector
              .map( _.processes.foldMap( id => env.processes.get( id ).foldMap( _.consumed( env.item ) ) ) ),
            priorities
          )
          .flatMap( flows =>
            flows.zipWithIndex
              .flatMap { case ( m, i ) => m.map { case ( j, d ) => ( i, j, d ) } }
              .toNev
              .map( _.map {
                case ( i, j, amt ) =>
                  Transport(
                    amt,
                    producersV( i ) match {
                      case ( id, idsOpt ) => idsOpt.getOrElse( NonEmptyVector.one( id ) )
                    },
                    consumers.toVector( j ).processes
                  )
              } )
              .liftTo[F]( ItemFlow.DummyError )
          )
      }
    //      ItemFlow.Env.withBalancer { fb =>
//        producers.toSortedMap.keys.toVector
//          .traverse( id => ItemFlow.Env.getProcess( id ).map( _.foldMap( _.produced() ) ) )
//        fb(
//          )
//      }

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

  }

  object ItemFlow {

    private object DummyError extends Throwable( "", null, false, false )

    case class Env[F[_]]( item: Item, processes: SortedMap[ProcessId, Process], planEnv: FactoryPlan.Env[F] )

    object Env {
      def getProcess[F[_]]( id: ProcessId ): StateT[F, Env[F], Option[Process]] = ???

      def removeProcesses[F[_]]( ids: Iterable[ProcessId] ): State[Env[F], Unit] =
        State.modify( env => env.copy( processes = env.processes.removedAll( ids ) ) )

      def register[F[_]]( process: Process ): State[Env[F], ProcessId] = {
        lift( FactoryPlan.Env.getNextId[F] )
          .flatTap( id => State.modify( env => env.copy( processes = env.processes.updated( id, process ) ) ) )
          .widen[ProcessId]
      }

      def withBalancer[F[_]: Monad, A]( k: FlowBalancer[F] => F[A] ): StateT[F, Env[F], A] =
        lift( FactoryPlan.Env.withBalancer( k ) )

      private def lift[F[_], G[_]: Functor, A]( fa: StateT[G, FactoryPlan.Env[F], A] ): StateT[G, Env[F], A] =
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
