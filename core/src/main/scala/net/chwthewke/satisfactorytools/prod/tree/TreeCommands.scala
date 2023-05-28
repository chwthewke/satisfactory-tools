package net.chwthewke.satisfactorytools
package prod.tree

import cats.Eval
import cats.Monoid
import cats.Show
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.partialOrder._
import cats.syntax.semigroup._
import io.circe.Decoder
import io.circe.Encoder

case class TreeCommands( commands: Vector[TreeCommand] ) extends AnyVal {
  def append( command: TreeCommand ): TreeCommands =
    this |+| TreeCommands( command )
}

object TreeCommands {
  val empty: TreeCommands = TreeCommands( Vector.empty )

  def apply( command: TreeCommand ): TreeCommands = TreeCommands( Vector( command ) )

  implicit val treeCommandsMonoid: Monoid[TreeCommands] =
    new Monoid[TreeCommands] {
      override def empty: TreeCommands = TreeCommands.empty

      override def combine( x: TreeCommands, y: TreeCommands ): TreeCommands =
        TreeCommands( y.commands.foldLeft( x.commands )( appendCommand ) )
    }

  implicit val treeCommandsDecoder: Decoder[TreeCommands] = Decoder[Vector[TreeCommand]].map( TreeCommands( _ ) )
  implicit val treeCommandsEncoder: Encoder[TreeCommands] = Encoder[Vector[TreeCommand]].contramap( _.commands )

  implicit val treeCommandsShow: Show[TreeCommands] =
    Show.show( _.commands.mkString_( " | " ) )

  def appendCommand[C >: TreeCommand.PushDown <: TreeCommand](
      commands: Vector[C],
      command: TreeCommand
  ): Vector[C] =
    command match {
      case TreeCommand.PullUp( from, recipe ) =>
        // TODO this recursion could be a filter (but it's also wrong)
        ( commands, Vector.empty[C] )
          .tailRecM[Eval, Vector[C]] {
            case ( rest, acc ) =>
              if (rest.isEmpty)
                Eval.now( Right( acc ) )
              else {
                rest.last match {
                  case TreeCommand.PushDown( to, recipe1, _ ) if (from: TreeLoc) == to && recipe == recipe1 =>
                    Eval.later( Left( ( rest.init, acc ) ) )
                  case _ =>
                    Eval.later( Left( ( rest.init, rest.last +: acc ) ) )
                }
              }
          }
          .value

      case TreeCommand.Destroy( at ) =>
        ( commands, Vector.empty[C] )
          .tailRecM[Eval, Vector[C]] {
            case ( rest, acc ) =>
              if (rest.isEmpty)
                Eval.now( Right( acc ) )
              else {
                rest.last match {
                  case TreeCommand.PushDown( to, _, _ ) if (at: TreeLoc) <= to =>
                    Eval.later( Left( ( rest.init, acc ) ) )
                  case _ =>
                    Eval.later( Left( ( rest.init, rest.last +: acc ) ) )
                }
              }
          }
          .value
      case c @ TreeCommand.PushDown( _, _, _ ) => commands :+ c
    }
}
