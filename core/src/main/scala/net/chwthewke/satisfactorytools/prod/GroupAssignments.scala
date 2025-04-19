package net.chwthewke.satisfactorytools
package prod

import cats.Eq
import cats.Show
import cats.data.NonEmptyVector
import cats.derived.semiauto
import cats.syntax.all._

final case class GroupAssignments[A]( groups: Vector[GroupAssignment[A]] ) {
  def count: Int = groups.size

  def indices: IndexedSeq[Int] = groups.indices.map( _ + 1 )

  def at( n: Int ): Option[GroupAssignment[A]] =
    groups.lift( n - 1 )

  def groupIndices: Map[A, ( Int, Int, Boolean )] =
    groups.iterator.zipWithIndex.flatMap {
      case ( group, grpIx ) =>
        group.withIndices.map { case ( item, ix, sectionBefore ) => ( item, ( grpIx + 1, ix, sectionBefore ) ) }
    }.toMap

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
  def update( newAssignments: Map[A, Int] )( implicit eq: Eq[A] ): GroupAssignments[A] = GroupAssignments(
    groups.zipWithIndex.map {
      case ( g, ix ) =>
        val kept: GroupAssignment[A] = g.filter( newAssignments.get( _ ).contains( ix + 1 ) )
        val added: Iterable[A]       = newAssignments.filter { _._2 == ix + 1 }.keys.filterNot( kept.contains )
        added.foldLeft( kept )( _.append( _, sectionBefore = false ) )
    }
  )

}

object GroupAssignments {
  implicit def groupAssignmentsShow[A: Show]: Show[GroupAssignments[A]] =
    semiauto.show[GroupAssignments[A]]

  def empty[A]: GroupAssignments[A] = GroupAssignments( Vector.empty )

  def emptyGroups[A]( groupCount: Int ): GroupAssignments[A] =
    GroupAssignments( Vector.fill( groupCount )( GroupAssignment.empty[A] ) )

  def of[A: Eq]( groupCount: Int, assignments: Map[A, Int] ): GroupAssignments[A] =
    emptyGroups( groupCount ).update( assignments )
}

final case class GroupAssignment[A]( sections: Vector[NonEmptyVector[A]] ) extends AnyVal {
  def allRecipes: Vector[A] = sections.flatMap( _.toVector )

  def filter( p: A => Boolean ): GroupAssignment[A] =
    GroupAssignment( sections.mapFilter { section =>
      val newSection: Vector[A] = section.filter( p )
      newSection.toNev
    } )

  def append( recipe: A, sectionBefore: Boolean ): GroupAssignment[A] = {
    val ( initSections: Vector[NonEmptyVector[A]], lastSection: Option[NonEmptyVector[A]] ) =
      if (sections.isEmpty || sectionBefore) ( sections, None )
      else ( sections.init, Some( sections.last ) )

    GroupAssignment( initSections :+ lastSection.fold( NonEmptyVector.one( recipe ) )( _.append( recipe ) ) )
  }

  def contains( recipe: A )( implicit eq: Eq[A] ): Boolean = sections.exists( _.contains_( recipe ) )

  def withIndices: Iterator[( A, Int, Boolean )] =
    sections.iterator.zipWithIndex
      .flatMap {
        case ( section, secIx ) =>
          ( section.head, secIx != 0 ) +: section.tail.map( ( _, false ) )
      }
      .zipWithIndex
      .map { case ( ( item, sectionBefore ), ix ) => ( item, ix, sectionBefore ) }
}

object GroupAssignment {
  implicit def groupAssignmentShow[A: Show]: Show[GroupAssignment[A]] =
    semiauto.show[GroupAssignment[A]]

  def empty[A]: GroupAssignment[A] = GroupAssignment( Vector.empty[NonEmptyVector[A]] )
}
