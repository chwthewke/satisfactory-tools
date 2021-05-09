package net.chwthewke.satisfactorytools
package web.state

import cats.Show
import enumeratum.Enum
import enumeratum.EnumEntry
import org.http4s.FormDataDecoder
import scalatags.Text
import shapeless.Lens
import shapeless.lens

import model.Bill
import model.ResourceOptions
import model.Model
import model.Options
import model.RecipeList
import model.SolverInputs
import web.protocol.Forms
import web.view.BillView
import web.view.ResourceOptionsView
import web.view.OptionsView
import web.view.RecipeListView

sealed abstract class InputTab( val id: String ) extends EnumEntry with Product {
  type Data

  override val entryName: String = id

  def decoder( model: Model ): FormDataDecoder[Data]

  def view( model: Model, state: Data ): Text.TypedTag[String]

  def stateLens: Lens[SolverInputs, Data]
}

object InputTab extends Enum[InputTab] {

  final case object BillTab extends InputTab( "bill" ) {
    type Data = Bill

    override def decoder( model: Model ): FormDataDecoder[Bill] = Forms.bill( model )

    override def view( model: Model, state: Bill ): Text.TypedTag[String] = BillView.view( model, state )

    override def stateLens: Lens[SolverInputs, Bill] = lens[SolverInputs].bill
  }

  final case object RecipesTab extends InputTab( "recipes" ) {
    type Data = RecipeList

    override def decoder( model: Model ): FormDataDecoder[RecipeList] = Forms.recipeList( model )

    override def view( model: Model, state: RecipeList ): Text.TypedTag[String] = RecipeListView.view( model, state )

    override def stateLens: Lens[SolverInputs, RecipeList] = lens[SolverInputs].recipeList
  }

  final case object ResourceOptionsTab extends InputTab( "map" ) {
    type Data = ResourceOptions

    override def decoder( model: Model ): FormDataDecoder[ResourceOptions] = Forms.resourceOptions( model )

    override def view( model: Model, state: ResourceOptions ): Text.TypedTag[String] =
      ResourceOptionsView.view( model, state )

    override def stateLens: Lens[SolverInputs, ResourceOptions] = lens[SolverInputs].resourceOptions
  }

  final case object OptionsTab extends InputTab( "options" ) {
    override type Data = Options

    override def decoder( model: Model ): FormDataDecoder[Options] = Forms.options

    override def view( model: Model, state: Options ): Text.TypedTag[String] = OptionsView.view( state )

    val stateLens: Lens[SolverInputs, Options] = lens[SolverInputs].options
  }

  override val values: Vector[InputTab] = findValues.toVector

  implicit val inputTabShow: Show[InputTab] = Show.fromToString
}
