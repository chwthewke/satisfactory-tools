package net.chwthewke.dsptools

import cats.Show
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.show._
import scodec.Decoder
import scodec.codecs._
import scodec.interop.cats._

case class TechProto(
    name: String,
    id: Int,
    sId: String,
    desc: String,
    conclusion: String,
    published: Boolean,
    level: Int,
    maxLevel: Int,
    levelCoef1: Int,
    levelCoef2: Int,
    iconPath: String,
    isLabTech: Boolean,
    preTechs: Vector[Int],
    preTechsImplicit: Vector[Int],
    items: Vector[Int],
    itemPoints: Vector[Int],
    hashNeeded: Long,
    unlockRecipes: Vector[Int],
    unlockFunctions: Vector[Int],
    unlockValues: Vector[Double],
    addItems: Vector[Int],
    addItemCounts: Vector[Int],
    position: ( Float, Float )
)

object TechProto extends Decoders {
  implicit val decoder: Decoder[TechProto] = for {
    name             <- alignedUtf8.withLog( "tech name" )
    id               <- int32L.withLog( "tech id" )
    sId              <- alignedUtf8.withLog( "tech sId" )
    desc             <- alignedUtf8.withLog( "tech desc" )
    conclusion       <- alignedUtf8.withLog( "conclusion" )
    published        <- dspBool.withLog( "published" )
    level            <- int32L.withLog( "level" )
    maxLevel         <- int32L.withLog( "max level" )
    levelCoef1       <- int32L.withLog( "level coef 1" )
    levelCoef2       <- int32L.withLog( "level coef 2" )
    iconPath         <- alignedUtf8.withLog( "icon path" )
    isLabTech        <- dspBool.withLog( "is lab tech" )
    preTechs         <- decodeVector( int32L ).withLog( "pre techs" )
    preTechsImplicit <- decodeVector( int32L ).withLog( "pre techs implicit" )
    items            <- decodeVector( int32L ).withLog( "items" )
    itemPoints       <- decodeVector( int32L ).withLog( "item points" )
    hashNeeded       <- int64L.withLog( "hashes needed" )
    unlockRecipes    <- decodeVector( int32L ).withLog( "unlock recipes" )
    unlockFunctions  <- decodeVector( int32L ).withLog( "unlock functions" )
    unlockValues     <- decodeVector( double ).withLog( "unlock values" )
    addItems         <- decodeVector( int32L ).withLog( "add items" )
    addItemCounts    <- decodeVector( int32L ).withLog( "add item counts" )
    position         <- ( float.asDecoder, float.asDecoder ).tupled
  } yield TechProto(
    name,
    id,
    sId,
    desc,
    conclusion,
    published,
    level,
    maxLevel,
    levelCoef1,
    levelCoef2,
    iconPath,
    isLabTech,
    preTechs,
    preTechsImplicit,
    items,
    itemPoints,
    hashNeeded,
    unlockRecipes,
    unlockFunctions,
    unlockValues,
    addItems,
    addItemCounts,
    position
  )

  implicit val techProtoShow: Show[TechProto] = Show.show {
    case TechProto(
        name,
        id,
        sId,
        desc,
        conclusion,
        published,
        level,
        maxLevel,
        levelCoef1,
        levelCoef2,
        iconPath,
        isLabTech,
        preTechs,
        preTechsImplicit,
        items,
        itemPoints,
        hashNeeded,
        unlockRecipes,
        unlockFunctions,
        unlockValues,
        addItems,
        addItemCounts,
        position
        ) =>
      show"""ID:             $id
            |Name:           $name
            |sID:            $sId
            |desc:           $desc
            |conclusion:     $conclusion
            |published:      $published
            |level:          $level
            |max level:      $maxLevel
            |level coef. 1:  $levelCoef1
            |level coef. 2:  $levelCoef2
            |icon path:      $iconPath
            |is lab tech:    $isLabTech
            |pre techs:      $preTechs
            |pre techs impl: $preTechsImplicit
            |Items:          ${items.mkString_( ", " )}
            |Item Points :   ${itemPoints.mkString_( ", " )}
            |Hash Needed:    $hashNeeded
            |Unlock Recipes: ${unlockRecipes.mkString_( ", " )}
            |Unlock Funcs:   ${unlockFunctions.mkString_( ", " )}
            |Unlock Values:  ${unlockValues.mkString_( ", " )}
            |Add Items:      ${addItems.mkString_( ", " )}
            |Add Item Cts:   ${addItemCounts.mkString_( ", " )}
            |Position:       $position
            |""".stripMargin
  }
}
