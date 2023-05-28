package net.chwthewke.satisfactorytools
package prod
package tree

import cats.Show
import cats.derived.semiauto
import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import data.ClassName

sealed trait TreeCommand

object TreeCommand {
  case class PushDown( to: TreeLoc.NonRoot, recipe: ClassName, `type`: PushDownType ) extends TreeCommand
  case class PullUp( from: TreeLoc.NonRoot, recipe: ClassName )                       extends TreeCommand
  case class Destroy( at: TreeLoc.NonRoot )                                           extends TreeCommand

  implicit val treeCommandDecoder: Decoder[TreeCommand] = deriveDecoder[TreeCommand]
  implicit val treeCommandEncoder: Encoder[TreeCommand] = deriveEncoder[TreeCommand]

  implicit val treeCommandShow: Show[TreeCommand] = semiauto.show[TreeCommand]
}
