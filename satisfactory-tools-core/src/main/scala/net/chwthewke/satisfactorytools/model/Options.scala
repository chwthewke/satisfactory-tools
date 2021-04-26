package net.chwthewke.satisfactorytools
package model

import enumeratum.Enum
import enumeratum.EnumEntry

case class Options(
    belt: Options.Belt,
    pipe: Options.Pipe,
    miner: Options.Miner,
    clockSpeed: Options.ClockSpeed,
    extractors: Set[Options.Extractors],
    preferFracking: Set[Options.Extractors]
) {

  // None: not allowed, Int: priority for resource
  def scoreExtractionRecipe( recipe: Recipe[Machine, Item] ): Option[Int] =
    Option.when( extractors.exists( _.allow( recipe.producedIn, miner ) ) ) {
      if (Options.Extractors.WellExtractor.allow( recipe.producedIn, miner ))
        0
      else if (preferFracking.exists( _.allow( recipe.producedIn, miner ) ))
        1
      else
        -1
    }
}

object Options {
  val full: Options =
    Options( Belt.BeltMk5, Pipe.PipeMk2, Miner.MinerMk3, ClockSpeed.ClockSpeed250, Extractors.values.toSet, Set.empty )
  val default: Options =
    Options( Belt.BeltMk4, Pipe.PipeMk2, Miner.MinerMk2, ClockSpeed.ClockSpeed100, Extractors.values.toSet, Set.empty )

  sealed abstract class Belt( val itemsPerMinute: Int ) extends EnumEntry

  object Belt extends Enum[Belt] {
    final case object BeltMk1 extends Belt( 60 )
    final case object BeltMk2 extends Belt( 120 )
    final case object BeltMk3 extends Belt( 270 )
    final case object BeltMk4 extends Belt( 480 )
    final case object BeltMk5 extends Belt( 780 )

    override val values: Vector[Belt] = findValues.toVector
  }

  sealed abstract class Pipe( val cubicMetersPerMinute: Int ) extends EnumEntry

  object Pipe extends Enum[Pipe] {
    final case object PipeMk1 extends Pipe( 300 )
    final case object PipeMk2 extends Pipe( 600 )

    override val values: Vector[Pipe] = findValues.toVector
  }

  sealed abstract class Miner( val extractorClass: ClassName ) extends EnumEntry {
    private def allowsClass( className: ClassName ): Boolean =
      className == extractorClass || Miner.values.forall( _.extractorClass != className )

    def allows( machine: Machine ): Boolean =
      allowsClass( machine.className )

    def allows( recipe: Recipe[Machine, Item] ): Boolean =
      allowsClass( recipe.producedIn.className )
  }

  object Miner extends Enum[Miner] {
    final case object MinerMk1 extends Miner( ClassName( "Build_MinerMk1_C" ) )
    final case object MinerMk2 extends Miner( ClassName( "Build_MinerMk2_C" ) )
    final case object MinerMk3 extends Miner( ClassName( "Build_MinerMk3_C" ) )

    override val values: Vector[Miner] = findValues.toVector
  }

  sealed abstract class ClockSpeed( val percent: Int ) extends EnumEntry

  object ClockSpeed extends Enum[ClockSpeed] {
    final case object ClockSpeed100 extends ClockSpeed( 100 )
    final case object ClockSpeed250 extends ClockSpeed( 250 )

    override val values: Vector[ClockSpeed] = findValues.toVector
  }

  sealed abstract class Extractors extends EnumEntry {
    def allow( machine: Machine, miner: Miner ): Boolean
  }

  sealed abstract class SingleExtractor( className: ClassName ) extends Extractors {
    override def allow( machine: Machine, miner: Miner ): Boolean = machine.className == className
  }

  object Extractors extends Enum[Extractors] {
    final case object WellExtractor  extends SingleExtractor( Extractor.frackingExtractorClass )
    final case object WaterExtractor extends SingleExtractor( Extractor.waterExtractorClass )
    final case object OilExtractor   extends SingleExtractor( Extractor.oilExtractorClass )
    final case object Miners extends Extractors {
      override def allow( machine: Machine, miner: Miner ): Boolean =
        machine.className == miner.extractorClass
    }

    override val values: Vector[Extractors] = findValues.toVector
  }
}
