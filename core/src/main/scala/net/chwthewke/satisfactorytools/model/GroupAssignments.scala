package net.chwthewke.satisfactorytools
package model

import cats.syntax.functor._
import cats.syntax.functorFilter._

final case class GroupAssignments[A]( groups: Vector[GroupAssignment[A]] ) {
  def count: Int = groups.size

  def indices: IndexedSeq[Int] = groups.indices.map( _ + 1 )

  def at( n: Int ): Option[GroupAssignment[A]] =
    groups.lift( n - 1 )

  def groupIndex( item: A ): Option[( Int, Int )] =
    groups.iterator.zipWithIndex
      .map { case ( g, i ) => ( i + 1, g.indexOf( item ) ) }
      .find( _._2 >= 0 )

  lazy val groupsByItem: Map[A, Int] =
    groups.zipWithIndex.flatMap { case ( g, ix ) => g.allRecipes.tupleRight( ix + 1 ) }.toMap

  def get( className: A ): Option[Int]               = groupsByItem.get( className )
  def getOrElse( className: A, orElse: => Int ): Int = groupsByItem.getOrElse( className, orElse )

  def filter( hasRecipe: A => Boolean ): GroupAssignments[A] =
    GroupAssignments( groups.map( _.filter( hasRecipe ) ) )

  /**
   * @param groupNumber
   *   starts at 1 (the way it is stored in DB). Must be in-bounds
   */
  def append( groupNumber: Int, recipe: A, sectionBefore: Boolean ): GroupAssignments[A] =
    GroupAssignments(
      groups
        .lift( groupNumber - 1 )
        .map( _.append( recipe, sectionBefore ) )
        .foldLeft( groups )( _.updated( groupNumber - 1, _ ) )
    )

  // TBT:
  // - update( newAssignments ).groups.flatten [has the same elements as] newAssignments.keys
  // - update( newAssignments ).groupIndex( recipeId ).map( _._1 ) === newAssignments.get( recipeId ) [forall recipeId]
  // - update( newAssignments ).assignments === newAssignments
  // NOTE does not keep sections
  def update( newAssignments: Map[A, Int] ): GroupAssignments[A] = GroupAssignments(
    groups.zipWithIndex.map {
      case ( g, ix ) =>
        val kept: GroupAssignment[A] = g.filter( newAssignments.get( _ ).contains( ix + 1 ) )
        val added: Iterable[A]       = newAssignments.filter { _._2 == ix + 1 }.keys.filterNot( kept.contains )
        added.foldLeft( kept )( _.append( _, sectionBefore = false ) )
    }
  )

}

object GroupAssignments {
  def empty[A]: GroupAssignments[A] = GroupAssignments( Vector.empty )

  def emptyGroups[A]( groupCount: Int ): GroupAssignments[A] =
    GroupAssignments( Vector.fill( groupCount )( GroupAssignment.empty[A] ) )

  def of[A]( groupCount: Int, assignments: Map[A, Int] ): GroupAssignments[A] =
    emptyGroups( groupCount ).update( assignments )
}

final case class GroupAssignment[A]( sections: Vector[Vector[A]] ) extends AnyVal {
  def allRecipes: Vector[A] = sections.flatten

  def filter( p: A => Boolean ): GroupAssignment[A] =
    GroupAssignment( sections.mapFilter { section =>
      val newSection: Vector[A] = section.filter( p )
      Option.when( newSection.nonEmpty )( newSection )
    } )

  def append( recipe: A, sectionBefore: Boolean ): GroupAssignment[A] = {
    val ( initSections, lastSection ) =
      if (sections.isEmpty || sectionBefore) ( sections, Vector.empty )
      else ( sections.init, sections.last )

    GroupAssignment( initSections :+ ( lastSection :+ recipe ) )
  }

  def contains( recipe: A ): Boolean = sections.exists( _.contains( recipe ) )
  def indexOf( recipe: A ): Int      = allRecipes.indexOf( recipe )
}

object GroupAssignment {
  def empty[A]: GroupAssignment[A] = GroupAssignment( Vector.empty[Vector[A]] )
}
