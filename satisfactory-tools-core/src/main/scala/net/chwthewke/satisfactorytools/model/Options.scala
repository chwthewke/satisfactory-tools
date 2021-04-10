package net.chwthewke.satisfactorytools
package model

import enumeratum.Enum
import enumeratum.EnumEntry

case class Options(
    belt: Options.Belt,
    miner: Options.Miner,
    clockSpeed: Options.ClockSpeed,
    fracking: Set[Options.LiquidResource]
) {
  def allowsExtractionRecipe( recipe: Recipe[Machine, Item] ): Boolean =
    miner.allows( recipe ) &&
      Options.LiquidResource.values
        .find( _.matches( recipe ) )
        .forall(
          liq => fracking( liq ) == recipe.producers.exists( _.className == Extractor.frackingExtractorClass )
        )
}

object Options {
  val full: Options    = Options( Belt.BeltMk5, Miner.MinerMk3, ClockSpeed.ClockSpeed250, Set.empty )
  val default: Options = Options( Belt.BeltMk4, Miner.MinerMk2, ClockSpeed.ClockSpeed100, Set.empty )

  sealed abstract class Belt( val itemsPerMinute: Int ) extends EnumEntry

  object Belt extends Enum[Belt] {
    final case object BeltMk1 extends Belt( 60 )
    final case object BeltMk2 extends Belt( 120 )
    final case object BeltMk3 extends Belt( 270 )
    final case object BeltMk4 extends Belt( 480 )
    final case object BeltMk5 extends Belt( 780 )

    override val values: Vector[Belt] = findValues.toVector
  }

  sealed abstract class Miner( val extractorClass: ClassName ) extends EnumEntry {
    private def allowsClass( className: ClassName ): Boolean =
      className == extractorClass || Miner.values.forall( _.extractorClass != className )

    def allows( extractor: Extractor ): Boolean =
      allowsClass( extractor.className )

    def allows( recipe: Recipe[Machine, Item] ): Boolean =
      recipe.producers.forall( m => allowsClass( m.className ) )
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

  sealed abstract class LiquidResource( val className: ClassName ) extends EnumEntry {
    def matches( recipe: Recipe[Machine, Item] ): Boolean =
      recipe.product.forall( _.item.className == className )

    def allows( recipe: Recipe[Machine, Item] ): Boolean =
      recipe.product.forall(
        p =>
          LiquidResource.values.forall( _.className != p.item.className ) ||
            p.item.className == className
      )

  }

  object LiquidResource extends Enum[LiquidResource] {
    final case object Water    extends LiquidResource( ClassName( "Desc_Water_C" ) )
    final case object CrudeOil extends LiquidResource( ClassName( "Desc_LiquidOil_C" ) )

    override val values: Vector[LiquidResource] = findValues.toVector
  }
}
