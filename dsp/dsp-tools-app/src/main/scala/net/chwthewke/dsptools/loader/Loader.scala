package net.chwthewke.dsptools
package loader

import cats.effect.IO
import cats.effect.Sync
import cats.syntax.applicativeError._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.vector._
import fs2.io.readClassLoaderResource
import scodec.Decoder
import scodec.bits.ByteVector

import net.chwthewke.factory.data.Countable
import gamedata.ItemProto
import gamedata.ProtoSet
import gamedata.RecipeProto
import gamedata.StringProto
import model.Item
import model.Model
import model.Recipe

class Loader[F[_]: Sync] {
  def loadProtoSetResource[P]( resource: String )( implicit decoder: Decoder[P] ): F[ProtoSet[P]] =
    readClassLoaderResource[F]( resource ).compile
      .to( ByteVector )
      .attemptT
      .subflatMap(
        bytes =>
          ProtoSet
            .decoder( decoder )
            .complete
            .decode( bytes.toBitVector )
            .toEither
            .leftMap( err => new IllegalArgumentException( err.messageWithContext.take( 32768 ) ) )
            .map( _.value )
      )
      .rethrowT

  def loadModel: F[Model] =
    for {
      loadStrings <- loadProtoSetResource[StringProto]( "StringProtoSet.dat" )
      loadItems   <- loadProtoSetResource[ItemProto]( "ItemProtoSet.dat" )
      loadRecipes <- loadProtoSetResource[RecipeProto]( "RecipeProtoSet.dat" )
    } yield {
      val stringsByName: Map[String, String] =
        loadStrings.protos.flatMap {
          case StringProto( name, _, _, zhCN, enUS, frFR ) =>
            Vector( enUS, frFR, zhCN, name ).find( _.nonEmpty ).tupleLeft( name )
        }.toMap

      val items: Vector[Item] =
        loadItems.protos.map( proto => Item( stringsByName.getOrElse( proto.name, proto.name ), proto ) )

      val itemsById: Map[Int, Item] = items.map( item => ( item.id, item ) ).toMap

      val recipes = loadRecipes.protos.flatMap { proto =>
        val displayName = stringsByName.getOrElse( proto.name, proto.name )
        val ingredients = proto.items
          .zip( proto.itemCounts )
          .flatMap { case ( id, count ) => itemsById.get( id ).map( Countable( _, count ) ) }
        val products = proto.results
          .zip( proto.resultCounts )
          .flatMap { case ( id, count ) => itemsById.get( id ).map( Countable( _, count ) ) }
          .toNev

        products.map( Recipe( displayName, ingredients, _, proto ) )
      }

      Model( items, recipes )

    }
}

object Loader {
  val io: Loader[IO] = new Loader[IO]
}
