package net.chwthewke.dsptools

import cats.Show
import cats.syntax.foldable._
import cats.syntax.show._
import scodec.codecs._
import scodec.Decoder

case class RecipeProto(
    name: String,
    id: Int,
    sId: String,
    recipeType: RecipeType,
    handcraft: Boolean,
    explicit: Boolean,
    timeSpend: Int,
    items: Vector[Int],
    itemCounts: Vector[Int],
    results: Vector[Int],
    resultCounts: Vector[Int],
    gridIndex: Int,
    iconPath: String,
    description: String
)

object RecipeProto extends Decoders {
  implicit val decoder: Decoder[RecipeProto] =
    for {
      name         <- alignedUtf8.withLog( "recipe name" )
      id           <- int32L.withLog( "recipe id" )
      sId          <- alignedUtf8.withLog( "recipe sId" )
      recipeType   <- RecipeType.decoder.withLog( "recipe type" )
      handcraft    <- dspBool.withLog( "handcraft" )
      explicit     <- dspBool.withLog( "explicit" )
      timeSpend    <- int32L.withLog( "time spend" )
      items        <- decodeVector( int32L ).withLog( "recipe items" )
      itemCounts   <- decodeVector( int32L ).withLog( "recipe item counts" )
      results      <- decodeVector( int32L ).withLog( "recipe results" )
      resultCounts <- decodeVector( int32L ).withLog( "recipe result counts" )
      gridIndex    <- int32L.withLog( "grid index" )
      iconPath     <- alignedUtf8.withLog( "icon path" )
      description  <- alignedUtf8.withLog( "description" )

    } yield RecipeProto(
      name,
      id,
      sId,
      recipeType,
      handcraft,
      explicit,
      timeSpend,
      items,
      itemCounts,
      results,
      resultCounts,
      gridIndex,
      iconPath,
      description
    )

  implicit val recipeProtoShow: Show[RecipeProto] = Show.show {
    case RecipeProto(
        name,
        id,
        sId,
        recipeType,
        handcraft,
        explicit,
        timeSpend,
        items,
        itemCounts,
        results,
        resultCounts,
        gridIndex,
        iconPath,
        description
        ) =>
      show"""ID:             $id
            |Name:           $name
            |sID:            $sId
            |Recipe Type:    $recipeType
            |handcraft:      $handcraft
            |explicit:       $explicit
            |time spend:     $timeSpend
            |Items:          ${items.mkString_( ", " )}
            |Item Counts:    ${itemCounts.mkString_( ", " )}
            |Results:        ${results.mkString_( ", " )}
            |Result Counts:  ${resultCounts.mkString_( ", " )}
            |grid index:     $gridIndex
            |icon path:      $iconPath
            |decsription:    $description
            |""".stripMargin

  }

}
