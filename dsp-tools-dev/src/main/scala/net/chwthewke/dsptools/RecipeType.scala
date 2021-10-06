package net.chwthewke.dsptools

import enumeratum.CatsEnum
import enumeratum.Enum
import enumeratum.EnumEntry
import scodec.Attempt
import scodec.Decoder
import scodec.Err
import scodec.codecs._

sealed abstract class RecipeType( val index: Int ) extends EnumEntry

object RecipeType extends Enum[RecipeType] with CatsEnum[RecipeType] with Decoders {
  case object None        extends RecipeType( 0 )
  case object Smelt       extends RecipeType( 1 )
  case object Chemical    extends RecipeType( 2 )
  case object Refine      extends RecipeType( 3 )
  case object Assemble    extends RecipeType( 4 )
  case object Particle    extends RecipeType( 5 )
  case object Exchange    extends RecipeType( 6 )
  case object PhotonStore extends RecipeType( 7 )
  case object Fractionate extends RecipeType( 8 )
  case object Research    extends RecipeType( 15 )

  override val values: Vector[RecipeType] = findValues.toVector

  implicit lazy val decoder: Decoder[RecipeType] =
    int32L
      .emap( n => Attempt.fromOption( values.find( _.index == n ), Err( s"Invalid index $n for enum RecipeType" ) ) )
      .withToString( s"enum RecipeType from $int32L" )
}
