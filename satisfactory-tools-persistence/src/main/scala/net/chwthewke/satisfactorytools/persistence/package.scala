package net.chwthewke.satisfactorytools

import cats.syntax.traverse._
import doobie._
import doobie.postgres.implicits._
import doobie.util.invariant.InvalidEnum
import enumeratum.Enum
import enumeratum.EnumEntry
import org.tpolecat.typename.TypeName
import scala.concurrent.duration._

import data.Form
import model.ExtractorType
import model.MachineType
import model.Options
import model.ResourcePurity

package object persistence {

  val ModelVersion: Int = 1

  private def enumMeta[A <: EnumEntry: TypeName]( pgEnumName: String )( implicit E: Enum[A] ): Meta[A] =
    pgEnumStringOpt( pgEnumName, E.withNameOption, _.entryName )

  private def fromPgEnumArray[A <: EnumEntry: TypeName]( arr: Array[String] )( implicit E: Enum[A] ): Vector[A] =
    arr.toVector
      .traverse( str => E.withNameOption( str ).toRight( InvalidEnum[A]( str ) ) )
      .fold( throw _, identity )

  private def toPgEnumArray[A <: EnumEntry]( enums: Vector[A] ): Array[String] =
    enums.map( _.entryName ).toArray

  def enumArrayMeta[A <: EnumEntry: TypeName]( pgEnumName: String )( implicit E: Enum[A] ): Meta[Vector[A]] =
    Meta.Advanced
      .array[String]( pgEnumName, s"_$pgEnumName" )
      .timap( fromPgEnumArray( _ ) )( toPgEnumArray( _ ) )

  implicit val formMeta: Meta[Form]                   = enumMeta( "T_FORM" )
  implicit val extractorTypeMeta: Meta[ExtractorType] = enumMeta( "T_EXTRACTOR_TYPE" )
  implicit val machineTypeMeta: Meta[MachineType]     = enumMeta( "T_MACHINE_TYPE" )
  implicit val purityMeta: Meta[ResourcePurity]       = enumMeta( "T_PURITY" )

  implicit val beltOptionsMeta: Meta[Options.Belt]            = enumMeta( "T_BELT_TIER" )
  implicit val pipeOptionMeta: Meta[Options.Pipe]             = enumMeta( "T_PIPE_TIER" )
  implicit val minerOptionMeta: Meta[Options.Miner]           = enumMeta( "T_MINER_TIER" )
  implicit val clockSpeedOptionMeta: Meta[Options.ClockSpeed] = enumMeta( "T_CLOCK_SPEED" )
  implicit val extractorTypeArrayMeta: Meta[Set[ExtractorType]] =
    enumArrayMeta[ExtractorType]( "T_EXTRACTOR_TYPE" ).timap( _.toSet )( _.toVector )

  implicit val finiteDurationMeta: Meta[FiniteDuration] =
    Meta[Int].timap( _.millis )( _.toMillis.toInt )

}
