package net.chwthewke.satisfactorytools
package model

import atto._
import Atto._
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
        (stringOf1( bpNoSep ) ~ char( '/' )).skipMany1 ~>
        stringOf1( bpNoSep ) ~> char( '.' ) ~>
        stringOf1( bpNoSep ) <~
        string( "\"'" )
    ).map( ClassName( _ ) )

  val buildableClass: Parser[ClassName] =
    (
      string( "/" ) ~>
        (stringOf1( bpNoSep ) ~ char( '/' )).skipMany1 ~>
        stringOf1( bpNoSep ) ~> char( '.' ) ~>
        stringOf1( bpNoSep )
    ).map( ClassName( _ ) )

  val buildablesList: Parser[NonEmptyList[ClassName]] =
    char( '(' ) ~> buildableClass.sepBy1( char( ',' ) ) <~ char( ')' )

  val countable: Parser[Countable[ClassName, Int]] =
    ((string( "(ItemClass=" ) ~> bpGeneratedClass <~ string( ",Amount=" )) ~ int <~ char( ')' ))
      .map( (Countable[ClassName, Int] _).tupled )

  val countableList: Parser[NonEmptyList[Countable[ClassName, Int]]] =
    char( '(' ) ~> countable.sepBy1( char( ',' ) ) <~ char( ')' )

  def listOf[A]( p: Parser[A] ): Parser[List[A]]          = char( '(' ) ~> p.sepBy( char( ',' ) ) <~ char( ')' )
  def listOf1[A]( p: Parser[A] ): Parser[NonEmptyList[A]] = char( '(' ) ~> p.sepBy1( char( ',' ) ) <~ char( ')' )

  def enum[A <: EnumEntry]( e: Enum[A] ): Parser[A] =
    e.values.foldLeft(
      err[A]( e.values.map( _.entryName ).mkString( s"Not one of: <", ", ", ">" ) )
    )( ( p, a ) => p | string( a.entryName ).as( a ) )

  implicit class ParserOps[A]( val self: Parser[A] ) {
    def decoder: Decoder[A] = Decoder[String].emap(
      x =>
        Parser
          .parseOnly( self, x )
          .either
          .leftMap( e => s"parsing $x as $self: $e" )
    )
  }

}
