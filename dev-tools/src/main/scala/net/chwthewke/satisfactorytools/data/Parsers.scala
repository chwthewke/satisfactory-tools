package net.chwthewke.satisfactorytools
package data

import atto.Atto._
import atto._
import cats.data.NonEmptyList
import cats.syntax.either._
import cats.syntax.functor._
import enumeratum.Enum
import enumeratum.EnumEntry
import io.circe.Decoder

object Parsers {

  private val classNameString = stringOf1( satisfy( c => c != ',' && c != ')' ) ).map( ClassName( _ ) )

  val classNameList: Parser[NonEmptyList[ClassName]] =
    char( '(' ) ~> classNameString.sepBy1( char( ',' ) ) <~ char( ')' )

  private val bpNoSep = satisfy( c => c != ',' && c != '.' && c != '/' && c != '"' && c != ')' )

  val bpGeneratedClass: Parser[ClassName] =
    (
      string( "BlueprintGeneratedClass'\"/Game/FactoryGame/" ) ~>
        (stringOf1( bpNoSep ) ~ char( '/' )).skipMany ~>
        stringOf1( bpNoSep ) ~> char( '.' ) ~>
        stringOf1( bpNoSep ) <~
        string( "\"'" )
    ).map( ClassName( _ ) )

  val bpGeneratedClassList: Parser[Vector[ClassName]] =
    listOf1( bpGeneratedClass ).map( _.toList.toVector )

  val buildableClass: Parser[ClassName] =
    (
      string( "/" ) ~>
        (stringOf1( bpNoSep ) ~ char( '/' )).skipMany1 ~>
        stringOf1( bpNoSep ) ~> char( '.' ) ~>
        stringOf1( bpNoSep )
    ).map( ClassName( _ ) )

  val buildablesList: Parser[List[ClassName]] =
    (char( '(' ) ~> buildableClass.sepBy1( char( ',' ) ) <~ char( ')' )).map( _.toList ) |
      ok( Nil )

  val countable: Parser[Countable[Double, ClassName]] =
    ((string( "(ItemClass=" ) ~> bpGeneratedClass <~ string( ",Amount=" )) ~ double <~ char( ')' ))
      .map( (Countable[Double, ClassName] _).tupled )

  val countableList: Parser[NonEmptyList[Countable[Double, ClassName]]] =
    char( '(' ) ~> countable.sepBy1( char( ',' ) ) <~ char( ')' )

  val texture2d: Parser[IconData] =
    (string( "Texture2D " ) ~>
      (char( '/' ) ~> stringOf1( bpNoSep )).many1 ~ (char( '.' ) ~> stringOf1( bpNoSep )))
      .map {
        case ( pkgPath, texture ) =>
          IconData( pkgPath.init.mkString( "/" ), pkgPath.last, texture )
      }

  def listOf[A]( p: Parser[A] ): Parser[List[A]]          = char( '(' ) ~> p.sepBy( char( ',' ) ) <~ char( ')' )
  def listOf1[A]( p: Parser[A] ): Parser[NonEmptyList[A]] = char( '(' ) ~> p.sepBy1( char( ',' ) ) <~ char( ')' )

  def `enum`[A <: EnumEntry]( e: Enum[A] ): Parser[A] =
    e.values.foldLeft(
      err[A]( e.values.map( _.entryName ).mkString( s"Not one of: <", ", ", ">" ) )
    )( ( p, a ) => p | string( a.entryName ).as( a ) )

  implicit class ParserOps[A]( private val self: Parser[A] ) {
    def decoder: Decoder[A] = Decoder[String].emap(
      x =>
        Parser
          .parseOnly( self, x )
          .either
          .leftMap( e => s"parsing $x as $self: $e" )
    )
  }

}
