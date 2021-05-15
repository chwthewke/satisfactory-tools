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
import model.ResourceOptions
import model.ResourceWeights
import model.SolverInputs
import prod.ClockedRecipe
import prod.Factory
import web.state.CustomGroupSelection

object Codecs {

  val floatAsDouble: Codec[Double] = float.xmap[Double]( _.toDouble, _.toFloat )

  def inputsCodec( model: Model ): Codec[SolverInputs] =
    (billCodec( model ) ~ recipeListCodec( model ) ~ optionsCodec ~ resoucesOptionsCodec( model ))
      .xmap(
        SolverInputs( _, _, _, _ ),
        inputs => inputs.bill ~ inputs.recipeList ~ inputs.options ~ inputs.resourceOptions
      )

  def countableCodec[N, A]( numCodec: Codec[N], codec: Codec[A] ): Codec[Countable[N, A]] =
    (codec ~ numCodec)
      .xmap( Countable[N, A]( _, _ ), ct => ct.item ~ ct.amount )

  def billCodec( model: Model ): Codec[Bill] =
    vectorOfN( uint8, countableCodec( floatAsDouble, itemIndexCodec( model ) ) )
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

  def resourceNodesCodec( model: Model ): Codec[Map[ExtractorType, Map[Item, ResourceDistrib]]] =
    vectorOfN( uint8, mapDataPointCodec( model ) )
      .xmap(
        v => v.groupMap( _._1 )( _._2 ).map { case ( exT, items ) => ( exT, items.toMap ) },
        _.toVector.flatMap { case ( exT, items ) => items.toVector.tupleLeft( exT ) }
      )

  def resourceWeightsCodec( model: Model ): Codec[ResourceWeights] =
    vectorOfN( uint4, itemIndexCodec( model ) ~ uint4 )
      .xmap( v => ResourceWeights( v.toMap ), _.weights.toVector.filter( _._2 != ResourceWeights.range ) )

  def resoucesOptionsCodec( model: Model ): Codec[ResourceOptions] =
    (resourceNodesCodec( model ) ~ resourceWeightsCodec( model ))
      .xmap( ResourceOptions( _, _ ), opts => ( opts.resourceNodes, opts.resourceWeights ) )

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

  def clockedRecipeCodec( recipeCodec: Codec[Recipe[Machine, Item]] ): Codec[ClockedRecipe] =
    (countableCodec( uint16, recipeCodec ) ~ floatAsDouble)
      .xmap(
        ClockedRecipe.overclocked( _, _ ),
        cr => cr.recipe ~ cr.clockSpeed
      )

  def factoryCodec( model: Model ): Codec[Factory] =
    (vectorOfN( uint8, clockedRecipeCodec( extractionRecipeCodec( model ) ) ) ~
      vectorOfN( uint8, countableCodec( floatAsDouble, manufacturingRecipeCodec( model ) ) ) ~
      vectorOfN( uint8, countableCodec( floatAsDouble, itemIndexCodec( model ) ) ) ~
      vectorOfN( uint8, countableCodec( floatAsDouble, itemIndexCodec( model ) ) ))
      .xmap(
        Factory( _, _, _, _ ),
        factory => factory.extraction ~ factory.manufacturing ~ factory.extraInputs ~ factory.extraOutputs
      )

  def customGroupsCodec( model: Model ): Codec[CustomGroupSelection] =
    (uint4 ~ vectorOfN( uint8, recipeCodec( model.manufacturingRecipes ) ~ uint4 ))
      .xmap(
        { case ( n, v ) => CustomGroupSelection( n, v.filter { case ( _, n ) => n != 0 }.toMap ) },
        cgs => ( cgs.count, cgs.customGroups.filter { case ( _, n ) => n != 0 }.toVector )
      )

}
