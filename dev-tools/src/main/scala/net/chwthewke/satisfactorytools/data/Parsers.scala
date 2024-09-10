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

  private val bpNoSep: Parser[Char] =
    satisfy( c => c != ',' && c != '.' && c != '/' && c != '"' && c != ')' && c != '\'' ).named( "bpNoSep" )

  val oldBpGeneratedClass: Parser[ClassName] =
    (
      opt( string( "/Script/Engine." ) ) ~>
        string( "BlueprintGeneratedClass'\"/Game/FactoryGame/" ) ~>
        ( stringOf1( bpNoSep ) ~ char( '/' ) ).skipMany ~>
        stringOf1( bpNoSep ) ~> char( '.' ) ~>
        stringOf1( bpNoSep ) <~
        string( "\"'" )
    ).map( ClassName( _ ) )

  val newBpGeneratedClass: Parser[ClassName] =
    (
      string( "\"/Script/Engine.BlueprintGeneratedClass'/Game/FactoryGame/" ) ~>
        ( stringOf1( bpNoSep ) ~ char( '/' ) ).skipMany ~>
        stringOf1( bpNoSep ) ~> char( '.' ) ~>
        stringOf1( bpNoSep ) <~
        string( "'\"" )
    ).map( ClassName( _ ) )

  val bpGeneratedClass: Parser[ClassName] = newBpGeneratedClass | oldBpGeneratedClass

  val bpGeneratedClassList: Parser[Vector[ClassName]] =
    listOf1( bpGeneratedClass ).map( _.toList.toVector )

  val manufacturerClass: Parser[ClassName] = {
    val classNameParser: Parser[String] =
      string( "/" ) ~>
        ( stringOf1( bpNoSep ) ~ char( '/' ) ).skipMany1 ~>
        stringOf1( bpNoSep ) ~> char( '.' ) ~>
        stringOf1( bpNoSep )

    choice( classNameParser, char( '"' ) ~> classNameParser <~ char( '"' ) )
      .map( ClassName( _ ) )
  }

  val manufacturerClassList: Parser[List[ClassName]] =
    ( char( '(' ) ~> manufacturerClass.sepBy1( char( ',' ) ) <~ char( ')' ) ).map( _.toList ) |
      ok( Nil )

  val countable: Parser[Countable[Double, ClassName]] =
    ( ( string( "(ItemClass=" ) ~> bpGeneratedClass <~ string( ",Amount=" ) ) ~ double <~ char( ')' ) )
      .map( ( Countable[Double, ClassName] _ ).tupled )

  val countableList: Parser[NonEmptyList[Countable[Double, ClassName]]] =
    char( '(' ) ~> countable.sepBy1( char( ',' ) ) <~ char( ')' )

  val countableListOrEmpty: Parser[List[Countable[Double, ClassName]]] =
    countableList.map( _.toList ) | string( "" ).as( Nil )

  val texture2d: Parser[IconData] =
    ( string( "Texture2D " ) ~>
      ( char( '/' ) ~> stringOf1( bpNoSep ) ).many1 ~ ( char( '.' ) ~> stringOf1( bpNoSep ) ) )
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

  val booleanString: Parser[Boolean] =
    string( "False" ).as( false ) | string( "True" ).as( true )

  implicit class ParserOps[A]( private val self: Parser[A] ) {
    def decoder: Decoder[A] = Decoder[String].emap( x =>
      Parser
        .parseOnly( self, x )
        .either
        .leftMap( e => s"parsing $x as $self: $e" )
    )
  }

}
