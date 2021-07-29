package net.chwthewke.satisfactorytools
package model

import cats.Eq
import cats.Show
import cats.derived.semiauto
import enumeratum.Enum
import enumeratum.EnumEntry

import data.ClassName

case class Options(
    belt: Options.Belt,
    pipe: Options.Pipe,
    miner: Options.Miner,
    clockSpeed: Options.ClockSpeed,
    extractors: Set[ExtractorType],
    preferFracking: Set[ExtractorType]
) {

  private def allowExtractor( extractorType: ExtractorType, recipe: Recipe ): Boolean =
    extractorType.dataKey
      .fold(
        _ => recipe.producedIn.className == miner.extractorClass,
        _ == recipe.producedIn.className
      )

  // None: not allowed, Int: priority for resource
  def scoreExtractionRecipe( recipe: Recipe ): Option[Int] =
    Option.when( extractors.exists( allowExtractor( _, recipe ) ) ) {
      if (allowExtractor( ExtractorType.FrackingExtractor, recipe ))
        0
      else if (preferFracking.exists( allowExtractor( _, recipe ) ))
        1
      else
        -1
    }
}

object Options {
  val full: Options =
    Options(
      Belt.BeltMk5,
      Pipe.PipeMk2,
      Miner.MinerMk3,
      ClockSpeed.ClockSpeed250,
      ExtractorType.values.toSet,
      Set.empty
    )

  val default: Options =
    Options(
      Belt.BeltMk5,
      Pipe.PipeMk2,
      Miner.MinerMk3,
      ClockSpeed.ClockSpeed100,
      ExtractorType.values.toSet,
      Set.empty
    )

  implicit val optionsShow: Show[Options] = semiauto.show[Options]
  implicit val optionsEq: Eq[Options]     = Eq.fromUniversalEquals[Options]

  sealed abstract class Belt( override val entryName: String, val itemsPerMinute: Int ) extends EnumEntry

  object Belt extends Enum[Belt] {
    final case object BeltMk1 extends Belt( "belt-mk1", 60 )
    final case object BeltMk2 extends Belt( "belt-mk2", 120 )
    final case object BeltMk3 extends Belt( "belt-mk3", 270 )
    final case object BeltMk4 extends Belt( "belt-mk4", 480 )
    final case object BeltMk5 extends Belt( "belt-mk5", 780 )

    override val values: Vector[Belt] = findValues.toVector

    implicit val beltShow: Show[Belt] = Show.fromToString
  }

  sealed abstract class Pipe( override val entryName: String, val cubicMetersPerMinute: Int ) extends EnumEntry

  object Pipe extends Enum[Pipe] {
    final case object PipeMk1 extends Pipe( "pipe-mk1", 300 )
    final case object PipeMk2 extends Pipe( "pipe-mk2", 600 )

    override val values: Vector[Pipe] = findValues.toVector

    implicit val pipeShow: Show[Pipe] = Show.fromToString
  }

  sealed abstract class Miner( override val entryName: String, val extractorClass: ClassName ) extends EnumEntry {
    private def allowsClass( className: ClassName ): Boolean =
      className == extractorClass || Miner.values.forall( _.extractorClass != className )

    def allows( machine: Machine ): Boolean =
      allowsClass( machine.className )

    def allows( recipe: Recipe ): Boolean =
      allowsClass( recipe.producedIn.className )
  }

  object Miner extends Enum[Miner] {
    final case object MinerMk1 extends Miner( "miner-mk1", ClassName( "Build_MinerMk1_C" ) )
    final case object MinerMk2 extends Miner( "miner-mk2", ClassName( "Build_MinerMk2_C" ) )
    final case object MinerMk3 extends Miner( "miner-mk3", ClassName( "Build_MinerMk3_C" ) )

    override val values: Vector[Miner] = findValues.toVector

    implicit val minerShow: Show[Miner] = Show.fromToString
  }

  sealed abstract class ClockSpeed( override val entryName: String, val percent: Int ) extends EnumEntry

  object ClockSpeed extends Enum[ClockSpeed] {
    final case object ClockSpeed100 extends ClockSpeed( "clock-speed-100", 100 )
    final case object ClockSpeed250 extends ClockSpeed( "clock-speed-250", 250 )

    override val values: Vector[ClockSpeed] = findValues.toVector

    implicit val clockSpeedShow: Show[ClockSpeed] = Show.fromToString
  }

}
