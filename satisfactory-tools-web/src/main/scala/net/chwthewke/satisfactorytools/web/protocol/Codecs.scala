package net.chwthewke.satisfactorytools
package web.protocol

import cats.syntax.functor._
import scodec.Attempt
import scodec.Codec
import scodec.Err
import scodec.codecs._

import model.Bill
import model.Countable
import model.ExtractorType
import model.Item
import model.Machine
import model.MapOptions
import model.Model
import model.Options
import model.Options.Belt
import model.Options.ClockSpeed
import model.Options.Extractors
import model.Options.Miner
import model.Options.Pipe
import model.Recipe
import model.RecipeList
import model.ResourceDistrib
import model.SolverInputs
import net.chwthewke.satisfactorytools.web.state.CustomGroupSelection
import prod.Factory
import prod.FactoryBlock

object Codecs {

  val floatAsDouble: Codec[Double] = float.xmap[Double]( _.toDouble, _.toFloat )

  def inputsCodec( model: Model ): Codec[SolverInputs] =
    (billCodec( model ) ~ recipeListCodec( model ) ~ optionsCodec ~ mapOptionsCodec( model ))
      .xmap(
        SolverInputs( _, _, _, _ ),
        inputs => inputs.bill ~ inputs.recipeList ~ inputs.options ~ inputs.mapOptions
      )

  def countableCodec[A]( codec: Codec[A] ): Codec[Countable[Double, A]] =
    (codec ~ floatAsDouble)
      .xmap( Countable[Double, A]( _, _ ), ct => ct.item ~ ct.amount )

  def billCodec( model: Model ): Codec[Bill] =
    vectorOfN( uint8, countableCodec( itemIndexCodec( model ) ) )
      .xmap( Bill( _ ), _.items )

  def recipeListCodec( model: Model ): Codec[RecipeList] =
    subsetCodec[Vector]( model.manufacturingRecipes )
      .xmap( RecipeList( _ ), _.recipes )

  val optionsCodec: Codec[Options] =
    (enumCodec[Belt] ~
      enumCodec[Pipe] ~
      enumCodec[Miner] ~
      enumCodec[ClockSpeed] ~
      enumSetCodec[Extractors] ~
      enumSetCodec[Extractors])
      .xmap(
        Options( _, _, _, _, _, _ ),
        options =>
          options.belt ~ options.pipe ~ options.miner ~ options.clockSpeed ~ options.extractors ~ options.preferFracking
      )

  val distribCodec: Codec[ResourceDistrib] =
    (uint16 ~ uint16 ~ uint16)
      .xmap(
        ResourceDistrib( _, _, _ ),
        rd => rd.impureNodes ~ rd.normalNodes ~ rd.pureNodes
      )

  def mapDataPointCodec( model: Model ): Codec[( ExtractorType, ( Item, ResourceDistrib ) )] =
    enumCodec[ExtractorType] ~~ (itemIndexCodec( model ) ~~ distribCodec)

  def mapOptionsCodec( model: Model ): Codec[MapOptions] =
    vectorOfN( uint8, mapDataPointCodec( model ) )
      .xmap(
        v => MapOptions( v.groupMap( _._1 )( _._2 ).map { case ( exT, items ) => ( exT, items.toMap ) } ),
        _.resourceNodes.toVector.flatMap { case ( exT, items ) => items.toVector.tupleLeft( exT ) }
      )

  val mapDataCodec: Codec[Vector[( Byte, Byte, Short, Short, Short )]] =
    vectorOfN( uint8, byte ~~ byte ~~ short16 ~~ short16 ~~ short16 )

  def itemIndexCodec( model: Model ): Codec[Item] = {
    val modelItems = model.items.values.toVector

    uint8.xmap( modelItems( _ ), modelItems.indexOf )
  }

  private def recipeCodec( recipes: Vector[Recipe[Machine, Item]] ): Codec[Recipe[Machine, Item]] =
    uint8
      .exmap(
        ix => Attempt.fromOption( recipes.lift( ix ), Err.General( s"recipe index OOB $ix", Nil ) ),
        rec =>
          Attempt.fromOption(
            Some( recipes.indexOf( rec ) ).filter( _ >= 0 ),
            Err.General( s"unknown recipe ${rec.displayName}", Nil )
          )
      )

  def extractionRecipeCodec( model: Model ): Codec[Recipe[Machine, Item]] =
    recipeCodec( model.extractionRecipes.map( _._3 ) )

  def manufacturingRecipeCodec( model: Model ): Codec[Recipe[Machine, Item]] =
    recipeCodec( model.manufacturingRecipes )

  def factoryBlockCodec( recipeCodec: Codec[Recipe[Machine, Item]] ): Codec[FactoryBlock] =
    (countableCodec( recipeCodec ) ~ floatAsDouble)
      .xmap(
        FactoryBlock( _, _ ),
        block => block.recipe ~ block.baseClock
      )

  def factoryCodec( model: Model ): Codec[Factory] =
    (vectorOfN( uint8, factoryBlockCodec( extractionRecipeCodec( model ) ) ) ~
      vectorOfN( uint8, factoryBlockCodec( manufacturingRecipeCodec( model ) ) ) ~
      vectorOfN( uint8, countableCodec( itemIndexCodec( model ) ) ) ~
      vectorOfN( uint8, countableCodec( itemIndexCodec( model ) ) ))
      .xmap(
        Factory( _, _, _, _ ),
        factory => factory.extraction ~ factory.manufacturing ~ factory.extraInputs ~ factory.extraOutputs
      )

  def customGroupsCodec( model: Model ): Codec[CustomGroupSelection] =
    vectorOfN( uint8, recipeCodec( model.manufacturingRecipes ) ~ uint4 )
      .xmap( v => CustomGroupSelection( v.toMap ), _.customGroups.toVector )

}
