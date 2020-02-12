package net.chwthewke.satisfactorytools

import atto._
import Atto._
import cats.data.NonEmptyList

object Parsers {

  private val classNameString = stringOf1( satisfy( c => c != ',' && c != ')' ) ).map( ClassName( _ ) )

  val classNameList: Parser[NonEmptyList[ClassName]] =
    char( '(' ) ~> classNameString.sepBy1( char( ',' ) ) <~ char( ')' )

  val countable: Parser[Countable[ClassName]] =
    ((string( "(ItemClass=" ) ~> classNameString <~ string( ",Amount=" )) ~ int <~ char( ')' ))
      .map( (Countable[ClassName] _).tupled )

  val countableList: Parser[NonEmptyList[Countable[ClassName]]] =
    char( '(' ) ~> countable.sepBy1( char( ',' ) ) <~ char( ')' )

}
