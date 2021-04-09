package net.chwthewke.satisfactorytools

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.either._
import io.circe.parser
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardOpenOption._

object GrabDocs extends IOApp {

  val defaultPath: String = "D:\\Games\\SatisfactoryExperimental"

  val destPath: Path = Paths.get( "satisfactory-tools-core", "src", "main", "resources", "Docs.json" )

  override def run( args: List[String] ): IO[ExitCode] = {

    val docsPath =
      Paths
        .get( args.headOption.getOrElse( defaultPath ) )
        .resolve( "CommunityResources" )
        .resolve( "Docs" )
        .resolve( "Docs.json" )

    for {
      contents <- IO.delay( Files.readString( docsPath, StandardCharsets.UTF_16 ) )
      json     <- parser.parse( contents ).liftTo[IO]
      _ <- IO.delay(
            Files.writeString( destPath, json.spaces2SortKeys, StandardCharsets.UTF_8, CREATE, TRUNCATE_EXISTING )
          )
    } yield ExitCode.Success

  }

}
