package net.chwthewke.satisfactorytools
package persistence

import cats.effect.Async
import cats.effect.kernel.Resource
import cats.syntax.all._
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import doobie.ExecutionContexts
import doobie.Transactor
import doobie.hikari.HikariTransactor
import javax.sql.DataSource
import org.flywaydb.core.Flyway

object Resources {

  def managedTransactor[F[_]: Async]( config: Config ): Resource[F, Transactor[F]] =
    transactor[F]( config )
      .evalMap { case ( ds, xa ) => flywayMigrate( ds ).as( xa ) }

  private def transactor[F[_]: Async]( config: Config ): Resource[F, ( DataSource, HikariTransactor[F] )] =
    for {
      connectEC <- ExecutionContexts.fixedThreadPool( 4 )
      ds        <- dataSource[F]( config )
    } yield ( ds, HikariTransactor[F]( ds, connectEC ) )

  private def hikariConfig( config: Config ): HikariConfig = {
    val c = new HikariConfig()
    c.setDataSourceClassName( "org.postgresql.ds.PGSimpleDataSource" )
    c.addDataSourceProperty( "serverName", "localhost" )
    c.addDataSourceProperty( "portNumber", 5432 )
    c.addDataSourceProperty( "databaseName", config.databaseName )
    c.addDataSourceProperty( "user", config.user )
    c.addDataSourceProperty( "password", config.password )
    c
  }

  private def dataSource[F[_]: Async]( config: Config ): Resource[F, HikariDataSource] =
    Resource.fromAutoCloseable( Async[F].delay( new HikariDataSource( hikariConfig( config ) ) ) )

  private def flywayMigrate[F[_]: Async]( ds: DataSource ): F[Unit] =
    Async[F].delay {
      Flyway
        .configure()
        .dataSource( ds )
        .locations( "classpath:net/chwthewke/satisfactorytools/persistence/migrations" )
        .load()
        .migrate()
    }.void

}
