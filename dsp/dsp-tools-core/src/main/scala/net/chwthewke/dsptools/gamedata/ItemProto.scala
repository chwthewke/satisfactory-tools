package net.chwthewke.dsptools
package gamedata

import cats.Show
import cats.syntax.foldable._
import cats.syntax.show._
import scodec.Decoder
import scodec.codecs._

case class ItemProto(
    name: String,
    id: Int,
    sId: String,
    `type`: ItemType,
    subId: Int,
    miningFrom: String,
    produceFrom: String,
    stackSize: Int,
    grade: Int,
    upgrades: Vector[Int],
    isFluid: Boolean,
    isEntity: Boolean,
    canBuild: Boolean,
    buildInGas: Boolean,
    iconPath: String,
    modelIndex: Int,
    modelCount: Int,
    hpMax: Int,
    ability: Int,
    heatValue: Long,
    potential: Long,
    reactorInc: Float,
    fuelType: Int,
    buildIndex: Int,
    buildMode: Int,
    gridIndex: Int,
    unlockKey: Int,
    preTechOverride: Int,
    productive: Boolean,
    mechaMaterialId: Int,
    descFields: Vector[Int],
    description: String
)

object ItemProto extends Decoders {
  implicit val decoder: Decoder[ItemProto] = for {
    name            <- alignedUtf8.withLog( "item name" )
    id              <- int32L.withLog( "item id" )
    sId             <- alignedUtf8.withLog( "item sId" )
    `type`          <- ItemType.decoder.withLog( "item type" )
    subId           <- int32L.withLog( "item sub-id" )
    miningFrom      <- alignedUtf8.withLog( "mining from" )
    produceFrom     <- alignedUtf8.withLog( "produce from" )
    stackSize       <- int32L.withLog( "stack size" )
    grade           <- int32L.withLog( "grade" )
    upgrades        <- decodeVector( int32L ).withLog( "upgrades" )
    isFluid         <- dspBool.withLog( "is fluid" )
    isEntity        <- dspBool.withLog( "is entity" )
    canBuild        <- dspBool.withLog( "can build" )
    buildInGas      <- dspBool.withLog( "build in gas" )
    iconPath        <- alignedUtf8.withLog( "icon path" )
    modelIndex      <- int32L.withLog( "model index" )
    modelCount      <- int32L.withLog( "model count" )
    hpMax           <- int32L.withLog( "HP max" )
    ability         <- int32L.withLog( "ability" )
    heatValue       <- int64L.withLog( "heat value" )
    potential       <- int64L.withLog( "potential" )
    reactorInc      <- float.withLog( "reactor increase" )
    fuelType        <- int32L.withLog( "fuel type" )
    buildIndex      <- int32L.withToString( "build index" )
    buildMode       <- int32L.withToString( "build mode" )
    gridIndex       <- int32L.withLog( "grid index" )
    unlockKey       <- int32L.withLog( "unlock key" )
    preTechOverride <- int32L.withLog( "pre tech override" )
    productive      <- dspBool.withLog( "productive" )
    mechaMaterialId <- int32.withLog( "mecha material id" )
    descFields      <- decodeVector( int32L ).withLog( "desc fields" )
    description     <- alignedUtf8.withLog( "description" )
  } yield ItemProto(
    name,
    id,
    sId,
    `type`,
    subId,
    miningFrom,
    produceFrom,
    stackSize,
    grade,
    upgrades,
    isFluid,
    isEntity,
    canBuild,
    buildInGas,
    iconPath,
    modelIndex,
    modelCount,
    hpMax,
    ability,
    heatValue,
    potential,
    reactorInc,
    fuelType,
    buildIndex,
    buildMode,
    gridIndex,
    unlockKey,
    preTechOverride,
    productive,
    mechaMaterialId,
    descFields,
    description
  )

  implicit val itemProtoShow: Show[ItemProto] = Show.show {
    case ItemProto(
        name,
        id,
        sId,
        typ,
        subId,
        miningFrom,
        produceFrom,
        stackSize,
        grade,
        upgrades,
        isFluid,
        isEntity,
        canBuild,
        buildInGas,
        iconPath,
        modelIndex,
        modelCount,
        hpMax,
        ability,
        heatValue,
        potential,
        reactorInc,
        fuelType,
        buildIndex,
        buildMode,
        gridIndex,
        unlockKey,
        preTechOverride,
        productive,
        mechaMaterialId,
        descFields,
        description
        ) =>
      show"""ID:             $id
            |Name:           $name
            |sID:            $sId
            |type:           $typ
            |sub-ID:         $subId
            |mining from:    $miningFrom
            |produce from:   $produceFrom
            |stack size:     $stackSize
            |grade:          $grade
            |upgrades:       ${upgrades.mkString_( ", " )}
            |is fluid:       $isFluid
            |is entity:      $isEntity
            |can build:      $canBuild
            |build in gas:   $buildInGas
            |icon path:      $iconPath
            |model index:    $modelIndex
            |model count:    $modelCount
            |HP max:         $hpMax
            |ability:        $ability
            |Heat Value:     $heatValue
            |potential:      $potential
            |Reactor incr.:  $reactorInc
            |fuel type:      $fuelType
            |build index:    $buildIndex
            |build mode:     $buildMode
            |grid index:     $gridIndex
            |unlock key:     $unlockKey
            |pre-tech ov.:   $preTechOverride,
            |productive:     $productive
            |mecha mat. id:  $mechaMaterialId
            |desc fields:    ${descFields.mkString_( ", " )}
            |description:    $description
            |""".stripMargin
  }

}
