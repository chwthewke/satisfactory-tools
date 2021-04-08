package net.chwthewke.satisfactorytools
package model

import alleycats.std.iterable._
import cats.Monoid
import cats.Show
import cats.data.NonEmptyList
import cats.data.Validated
import cats.data.ValidatedNel
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.functorFilter._
import cats.syntax.traverseFilter._
import cats.syntax.traverse._
import cats.syntax.show._
import cats.syntax.option._
import io.circe.Decoder
import mouse.boolean._
import mouse.option._

final case class ProtoModel(
    items: Map[ClassName, Item],
    extractors: Map[ClassName, Extractor],
    manufacturers: Map[ClassName, Manufacturer],
    recipes: Vector[Recipe[ClassName, ClassName]]
) {

  def toModel: ValidatedNel[String, Model] = {
    val ( selfExtr, reg ) = recipes.partition( isSelfExtraction )

    def validateItem( cn: ClassName ): ValidatedNel[String, Item] =
      items.get( cn ).toValidNel( show"Unknown item class $cn" )

    val selfExtraction: ValidatedNel[String, Vector[Recipe[Machine, Item]]] =
      extractors.values
        .flatMap(
          extractorRecipesFor( _, selfExtr.flatMap( recipe => items.get( recipe.product.head.item ) ) )
        )
        .toVector
        .traverse( _.traverse( validateItem ) )

    val extractedItems: Validated[NonEmptyList[String], Vector[Item]] =
      selfExtraction.map( _.map( _.product.head.item ).distinct )

    val regular: Validated[NonEmptyList[String], Vector[Recipe[Machine, Item]]] =
      reg.traverseFilter { recipe =>
        NonEmptyList
          .fromList( recipe.producers.filter( Manufacturer.builders ) )
          .traverse(
            producers =>
              (
                producers
                  .traverse(
                    cn =>
                      extractors
                        .get( cn )
                        .map( Machine.extractor )
                        .orElse( manufacturers.get( cn ).map( Machine.manufacturer ) )
                        .toValidNel( show"Unknown machine class $cn" )
                  ),
                recipe.traverse( validateItem )
              ).mapN(
                ( ps, rec ) => rec.copy( producers = ps.toList )
              )
          )
      }

    ( selfExtraction, regular, extractedItems ).mapN( ( se, rg, ei ) => Model( se, rg, items, ei ) )
  }

  def isSelfExtraction[M, N]( recipe: Recipe[M, N] ): Boolean =
    recipe.ingredients == List( recipe.product.head )

  def selfExtractibleResources: Iterable[Item] =
    recipes.mapFilter(
      recipe =>
        (recipe.ingredients == List( recipe.product.head )).option( () ) *>
          items.get( recipe.product.head.item )
    )

  def extractorRecipesFor( extractor: Extractor, resources: Iterable[Item] ): Iterable[Recipe[Machine, ClassName]] =
    extractor.allowedResources
      .cata(
        _.toList.flatMap( itemRecipe( extractor, _ ) ),
        resources
          .filter( item => extractor.allowedResourceForms.contains( item.form ) )
          .map( itemRecipe( extractor, _ ) )
      )

  def itemRecipe( extractor: Extractor, itemClassName: ClassName ): Option[Recipe[Machine, ClassName]] =
    items
      .get( itemClassName )
      .map( itemRecipe( extractor, _ ) )

  def itemRecipe( extractor: Extractor, item: Item ): Recipe[Machine, ClassName] =
    Recipe(
      ClassName( show"${item.className}_${extractor.className}" ),
      show"Extract ${item.displayName} with ${extractor.displayName}",
      Nil,
      NonEmptyList.of( Countable( item.className, extractor.itemsPerCycle ) ),
      extractor.cycleTime,
      Machine.extractor( extractor ) :: Nil
    )

}

object ProtoModel {
  val empty: ProtoModel = ProtoModel( Map.empty, Map.empty, Map.empty, Vector.empty )

  def items( items: Map[ClassName, Item] ): ProtoModel = ProtoModel( items, Map.empty, Map.empty, Vector.empty )
  def extractors( extractors: Map[ClassName, Extractor] ): ProtoModel =
    ProtoModel( Map.empty, extractors, Map.empty, Vector.empty )
  def manufacturers( manufacturers: Map[ClassName, Manufacturer] ): ProtoModel =
    ProtoModel( Map.empty, Map.empty, manufacturers, Vector.empty )
  def recipes( recipes: Vector[Recipe[ClassName, ClassName]] ): ProtoModel =
    ProtoModel( Map.empty, Map.empty, Map.empty, recipes )

  implicit val modelMonoid: Monoid[ProtoModel] = new Monoid[ProtoModel] {
    override def empty: ProtoModel = ProtoModel.empty

    override def combine( x: ProtoModel, y: ProtoModel ): ProtoModel =
      ProtoModel(
        x.items ++ y.items,
        x.extractors ++ y.extractors,
        x.manufacturers ++ y.manufacturers,
        x.recipes ++ y.recipes
      )
  }

  private def decodeMap[A]( dec: Decoder[A] )( f: A => ClassName ): Decoder[Map[ClassName, A]] =
    Decoder.decodeVector( dec ).map( v => v.map( x => f( x ) -> x ).to( Map ) )

  def modelClassDecoder( nativeClass: NativeClass ): Decoder[ProtoModel] =
    nativeClass match {
      case NativeClass.`partDescClass` | NativeClass.`consumableDescClass` | NativeClass.`nuclearFuelDescClass` =>
        decodeMap( Item.itemDecoder( ItemType.Part ) )( _.className ).map( ProtoModel.items )
      case NativeClass.`equipmentDescClass` =>
        decodeMap( Item.itemDecoder( ItemType.Equipment ) )( _.className ).map( ProtoModel.items )
      case NativeClass.`biomassDescClass` =>
        decodeMap( Item.itemDecoder( ItemType.Biomass ) )( _.className ).map( ProtoModel.items )
      case NativeClass.`resourceDescClass` =>
        decodeMap( Item.itemDecoder( ItemType.Resource ) )( _.className ).map( ProtoModel.items )
      case NativeClass.`manufacturerDescClass` =>
        decodeMap( Decoder[Manufacturer] )( _.className ).map( ProtoModel.manufacturers )
      case NativeClass.`resourceExtractorClass` | NativeClass.`waterPumpClass` =>
        decodeMap( Decoder[Extractor] )( _.className ).map( ProtoModel.extractors )
      case NativeClass.`recipeClass` =>
        Decoder[Vector[Recipe[ClassName, ClassName]]].map( ProtoModel.recipes )
      case _ => Decoder.const( ProtoModel.empty )
    }

  implicit val modelDecoder: Decoder[ProtoModel] =
    for {
      nativeClass <- Decoder[NativeClass].prepare( _.downField( "NativeClass" ) )
      model <- modelClassDecoder( nativeClass )
                .prepare( _.downField( "Classes" ) )
                .handleErrorWith(
                  f => Decoder.failed( f.withMessage( show"in NativeClass $nativeClass: ${f.message}" ) )
                )
    } yield model

  implicit val protoModelShow: Show[ProtoModel] = Show(
    model => show"""Recipes:
                   |
                   |${model.recipes.map( _.show ).intercalate( "\n" )}
                   |
                   |
                   |Items:
                   |
                   |${model.items.values.map( _.show ).intercalate( "\n" )}
                   |
                   |
                   |Extractors:
                   |
                   |${model.extractors.values.map( _.show ).intercalate( "\n" )}
                   |
                   |Manufacturers:
                   |
                   |${model.manufacturers.values.map( _.show ).intercalate( "\n" )}
                   |
                   |""".stripMargin
  )
}
