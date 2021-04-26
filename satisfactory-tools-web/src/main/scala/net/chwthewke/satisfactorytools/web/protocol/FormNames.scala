package net.chwthewke.satisfactorytools
package web.protocol

import cats.syntax.show._

import model.ExtractorType
import model.Item
import model.ResourcePurity

object FormNames {

  def billItem( item: Item ): String = show"bill_${item.className}"

  val recipes: String = "recipes"

  val optionsBeltKey: String  = "options_belt"
  val optionsPipeKey: String  = "options_pipe"
  val optionsMinerKey: String = "options_miner"
  val optionsClockKey: String = "options_clock"

  val optionsExtractorsKey: String = "options_extractors"
  val optionsFrackingKey: String   = "options_prefer_fracking"

  def extractorItemPurityKey( extractorType: ExtractorType, item: Item, purity: ResourcePurity ): String =
    s"distrib_${extractorType.entryName}_${item.className}_${purity.entryName}"

}
