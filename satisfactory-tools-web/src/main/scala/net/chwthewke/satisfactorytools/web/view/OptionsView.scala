package net.chwthewke.satisfactorytools
package web.view

import cats.syntax.show._
import enumeratum.Enum
import enumeratum.EnumEntry
import scalatags.Text
import scalatags.Text.all._

import model.Options
import model.Options.Belt
import model.Options.ClockSpeed
import model.Options.Extractors
import model.Options.Miner
import model.Options.Pipe
import web.protocol.Forms

object OptionsView {

  def view( options: Options ): Text.TypedTag[String] =
    fieldset(
      legend( "Options" ),
      div(
        "Belts",
        enumRadios[Belt]( Forms.optionsBeltKey, beltOptionLabel, options.belt )
      ),
      div(
        "Pipelines",
        enumRadios[Pipe]( Forms.optionsPipeKey, pipeOptionLabel, options.pipe )
      ),
      div(
        "Miners",
        enumRadios[Miner]( Forms.optionsMinerKey, minerOptionLabel, options.miner )
      ),
      div(
        "Extractor clock speed",
        enumRadios[ClockSpeed]( Forms.optionsClockKey, clockSpeedOptionLabel, options.clockSpeed )
      ),
      div(
        "Available extractors",
        enumSetCheckboxes[Extractors]( Forms.optionsExtractorsKey, extractorsOptionLabel, options.extractors )
      ),
      div(
        "Prefer fracking for",
        enumSetCheckboxes(
          Forms.optionsFrackingKey,
          extractorsOptionLabel,
          options.preferFracking,
          Some( Vector( Extractors.OilExtractor, Extractors.WaterExtractor ) )
        )
      )
    )

  def beltOptionLabel( belt: Belt ): String = {

    val head = belt match {
      case Belt.BeltMk1 => "Belt Mk. 1"
      case Belt.BeltMk2 => "Belt Mk. 2"
      case Belt.BeltMk3 => "Belt Mk. 3"
      case Belt.BeltMk4 => "Belt Mk. 4"
      case Belt.BeltMk5 => "Belt Mk. 5"
    }

    show"$head (${belt.itemsPerMinute} items / min.)"
  }

  def pipeOptionLabel( pipe: Pipe ): String = {
    val head = pipe match {
      case Pipe.PipeMk1 => "Pipeline Mk. 1"
      case Pipe.PipeMk2 => "Pipeline Mk. 2"
    }

    show"$head (${pipe.cubicMetersPerMinute} m³ / min.)"
  }

  def minerOptionLabel( miner: Miner ): String = miner match {
    case Miner.MinerMk1 => "Miner Mk. 1"
    case Miner.MinerMk2 => "Miner Mk. 2"
    case Miner.MinerMk3 => "Miner Mk. 3"
  }

  def clockSpeedOptionLabel( clockSpeed: ClockSpeed ): String = clockSpeed match {
    case ClockSpeed.ClockSpeed100 => "100%"
    case ClockSpeed.ClockSpeed250 => "250%"
  }

  def extractorsOptionLabel( extractor: Extractors ): String = extractor match {
    case Extractors.Miners         => "Miner"
    case Extractors.OilExtractor   => "Oil extractor"
    case Extractors.WaterExtractor => "Water extractor"
    case Extractors.WellExtractor  => "Resource Well Extractor"
  }

  def enumRadios[A <: EnumEntry]( key: String, describe: A => String, current: A )( implicit E: Enum[A] ): Frag = {
    def itemOption( item: A, selected: Boolean ): Text.TypedTag[String] = {
      val elId = show"${key}_${item.entryName}"

      div(
        input(
          `type` := "radio",
          id := elId,
          value := item.entryName,
          name := key,
          Option.when( selected )( checked )
        ),
        label( `for` := elId, describe( item ) )
      )
    }

    E.values.map( it => itemOption( it, it == current ) )
  }

  def enumSetCheckboxes[A <: EnumEntry](
      key: String,
      describe: A => String,
      current: Set[A],
      possible: Option[Vector[A]] = None
  )(
      implicit E: Enum[A]
  ): Frag = {
    def itemOption( item: A, selected: Boolean ): Text.TypedTag[String] = {
      val elId = show"${key}_${item.entryName}"

      div(
        input(
          `type` := "checkbox",
          id := elId,
          value := item.entryName,
          name := key,
          Option.when( selected )( checked )
        ),
        label( `for` := elId, describe( item ) )
      )
    }

    possible.getOrElse( E.values ).map( it => itemOption( it, current( it ) ) )

  }

}
