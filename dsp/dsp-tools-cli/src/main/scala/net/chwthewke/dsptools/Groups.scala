package net.chwthewke.dsptools

import cats.Monad
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.std.Random
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.show._

import net.chwthewke.factory.data.Countable
import loader.Loader
import prod.solver.ModifiedRecipe

object Groups extends IOApp {

  override def run( args: List[String] ): IO[ExitCode] = {
    val plan = Plan.plan13MallPlanet
    for {
      model <- Loader.io.loadModel
      ( _, solution ) <- MainSolve
                          .solve( model, plan )
                          .leftMap( err => new UnsupportedOperationException( s"No solution: $err" ) )
                          .liftTo[IO]
      random                              <- Random.scalaUtilRandom[IO]
      recipes                             <- random.shuffleVector( solution.recipes.filterNot( _.item.baseRecipe.id == 112 ) )
      ( iters, exhausted, result, score ) <- new RandomSearch[IO]( 5, random ).groups( plan, recipes )
      _                                   <- IO.println( show"After $iters iterations${if (exhausted) " EXHAUSTED" else ""}" )
      _                                   <- IO.println( showResult( result, score ) )
    } yield ExitCode.Success
  }

  private def showResult( result: Vector[Vector[Vector[ModifiedRecipe]]], score: Int ): String =
    show"""Best results with score = $score
          |
          |${result
            .map(
              groups =>
                groups.zipWithIndex
                  .map {
                    case ( group, ix ) =>
                      show"${ix + 1}: ${group.map( _.displayName ).mkString_( ", " )}"
                  }
                  .mkString_( "\n" )
            )
            .mkString_( "\n\n" )}
          |""".stripMargin

  class RandomSearch[F[_]: Monad]( size0: Int, random: Random[F] ) extends Search[F]( size0 ) {
    override def permutation( groupCount: Int ): F[( ( Int, Int ), ( Int, Int ) )] =
      (
        ( random.nextIntBounded( groupCount ), random.nextIntBounded( groupCount ) ).tupled,
        ( random.nextIntBounded( size ), random.nextIntBounded( size ) ).tupled
      ).tupled
  }

  abstract class Search[F[_]]( val size: Int, val depth: Int = 8, val maxIter: Int = 50000, val maxFail: Int = 100 )(
      implicit F: Monad[F]
  ) {

    def permutation( groupCount: Int ): F[( ( Int, Int ), ( Int, Int ) )]

//    private def tag( groups: Vector[Vector[ModifiedRecipe]] ): String =
//      groups.map( _.map( _.baseRecipe.id ).mkString_( "|" ) ).mkString( "||" )

    def groups(
        plan: Plan,
        recipes: Vector[Countable[Double, ModifiedRecipe]]
    ): F[( Int, Boolean, Vector[Vector[Vector[ModifiedRecipe]]], Int )] = {
      val initGroups = selectRecipes( plan, recipes )
      (
        0,
        0,
        Vector( initGroups ),
        Vector( initGroups ),
        score( initGroups ),
        Set.empty[Vector[Vector[ModifiedRecipe]]]
      ).tailRecM[F, ( Int, Boolean, Vector[Vector[Vector[ModifiedRecipe]]], Int )] {
        case ( i, f, groups, best, bestScore, seen ) =>
          if (groups.isEmpty || i >= maxIter || f >= maxFail)
            F.pure( Either.right( ( i, groups.isEmpty, best, bestScore ) ) )
          else
            next( groups.head ).map { extra =>
              val scored       = extra.filterNot( seen ).fproduct( score )
              val newBestScore = scored.map( _._2 ).min
              val bestExtra    = scored.filter( _._2 == newBestScore ).map( _._1 )
              Left(
                (
                  i + 1,
                  if (newBestScore >= bestScore) f + 1 else 0,
                  groups.tail ++ (if (newBestScore <= bestScore) bestExtra else Vector.empty),
                  if (newBestScore < bestScore) bestExtra.toVector else best ++ bestExtra,
                  newBestScore,
                  seen ++ scored.map( _._1 )
                )
              )
            }
      }
    }

    def selectRecipes(
        plan: Plan,
        recipes: Vector[Countable[Double, ModifiedRecipe]]
    ): Vector[Vector[ModifiedRecipe]] =
      recipes
        .map( _.item )
        .filter( _.products.exists( p => plan.bill.exists( bi => bi._1 == p.item.id ) ) )
        .grouped( size )
        .toVector

    def score( groups: Vector[Vector[ModifiedRecipe]] ): Int = score2( groups )

    def score1( groups: Vector[Vector[ModifiedRecipe]] ): Int =
      groups.foldMap( group => group.flatMap( _.ingredients.map( _.item.id ) ).distinct.size )

    def score2( groups: Vector[Vector[ModifiedRecipe]] ): Int =
      groups
        .foldLeft( ( 0, Set.empty[Int] ) ) {
          case ( ( s, av ), group ) =>
            val ings           = group.flatMap( _.ingredients.map( _.item.id ) ).distinct
            val ( pav, punav ) = ings.partition( av )
            val rest           = pav.drop( 3 ) ++ punav
            val gs             = rest.size
            ( s + gs, rest.toSet )
        }
        ._1

    def applyPermutations[A](
        groups: Vector[Vector[A]],
        perms: List[( ( Int, Int ), ( Int, Int ) )]
    ): Vector[Vector[A]] =
      perms.foldRight( groups ) { case ( ( gis, eis ), gs ) => applyPermutation( gs, gis, eis ) }

    def applyPermutation[A](
        groups: Vector[Vector[A]],
        groupIxs: ( Int, Int ),
        eltIxs: ( Int, Int )
    ): Vector[Vector[A]] = {
      if (groupIxs._1 == groupIxs._2)
        groups
      else
        (
          groups( groupIxs._1 ).lift( eltIxs._1 ),
          groups( groupIxs._2 ).lift( eltIxs._2 )
        ) match {
          case ( None, None ) =>
            groups
          case ( Some( x1 ), Some( x2 ) ) =>
            groups
              .updated( groupIxs._1, groups( groupIxs._1 ).updated( eltIxs._1, x2 ) )
              .updated( groupIxs._2, groups( groupIxs._2 ).updated( eltIxs._2, x1 ) )
          case ( Some( x1 ), None ) =>
            groups
              .updated( groupIxs._1, groups( groupIxs._1 ).patch( eltIxs._1, Vector.empty, 1 ) )
              .updated( groupIxs._2, groups( groupIxs._2 ) :+ x1 )
          case ( None, Some( _ ) ) =>
            applyPermutation( groups, groupIxs.swap, eltIxs.swap )
        }
    }

    def next( groups: Vector[Vector[ModifiedRecipe]] ): F[List[Vector[Vector[ModifiedRecipe]]]] = {
      (1 until depth).toVector
        .foldMapM( d => permutation( groups.length ).replicateA( d ).replicateA( groups.size * 2 ) )
        .map( _.map( ps => applyPermutations( groups, ps ) ) )
    }
  }

}
