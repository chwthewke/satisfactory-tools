package net.chwthewke.satisfactorytools

import cats.effect.Async
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Resource
import cats.effect.std.Console
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.show._
import doobie._
import doobie.implicits._
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax._

import loader.Loader
import model.Model
import persistence.Config
import persistence.WriteModel

class InitDatabaseModel[F[_]: Async]( loader: Loader[F] ) {

  private val console: Console[F] = Console.make[F]

  private val mkTransactor: Resource[F, ( String, Transactor[F] )] = for {
    config     <- Resource.eval( ConfigSource.default.at( "db" ).loadF[F, Config]() )
    transactor <- persistence.Resources.managedTransactor[F]( config )
  } yield ( config.databaseName, transactor )

  private def loadedModel( model: Model ): String =
    show"Loaded model ${model.version.name} with ${model.items.size} items" +
      show" and ${model.manufacturingRecipes.size} recipes."

  def loadModelToDatabase( storage: DataVersionStorage ): F[Unit] = {
    mkTransactor.use {
      case ( dbName, xa ) =>
        loader
          .loadModel( storage )
          .flatTap( model => console.println( loadedModel( model ) ) )
          .flatMap( model => WriteModel.writeModel( model, storage.modelVersion ).transact( xa ) )
          .flatTap( _ => console.println( show"Wrote model to database $dbName." ) )
    }.void
  }

}

object InitDatabaseModel {
  abstract class Program( storage: DataVersionStorage ) extends IOApp {
    override def run( args: List[String] ): IO[ExitCode] =
      new InitDatabaseModel[IO]( Loader.io ).loadModelToDatabase( storage ).as( ExitCode.Success )
  }
}

object InitDatabaseModelU4 extends InitDatabaseModel.Program( DataVersionStorage.Update4 )
object InitDatabaseModelU5 extends InitDatabaseModel.Program( DataVersionStorage.Update5 )
object InitDatabaseModelU6 extends InitDatabaseModel.Program( DataVersionStorage.Update6 )
