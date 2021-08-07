package net.chwthewke.satisfactorytools
package persistence
package plans

case class CustomGroupLists( groups: Vector[Vector[RecipeId]] ) {
  def count: Int = groups.size

  def groupIndex( recipeId: RecipeId ): Option[( Int, Int )] =
    groups.iterator.zipWithIndex
      .map { case ( g, i ) => ( i + 1, g.indexOf( recipeId ) ) }
      .find( _._2 >= 0 )

  // TBT:
  // - update( newAssignments ).groups.flatten [has the same elements as] newAssignments.keys
  // - update( newAssignments ).groupIndex( recipeId ).map( _._1 ) === newAssignments.get( recipeId ) [forall recipeId]
  // - update( newAssignemnts ).assignments === newAssignments
  def update( newAssignments: Map[RecipeId, Int] ): CustomGroupLists = CustomGroupLists(
    groups.zipWithIndex.map {
      case ( g, ix ) =>
        val kept  = g.filter( newAssignments.get( _ ).contains( ix + 1 ) )
        val added = newAssignments.filter { _._2 == ix + 1 }.keys.filterNot( kept.contains )
        kept ++ added
    }
  )

  def assignments: Map[RecipeId, Int] =
    groups.zipWithIndex.flatMap { case ( g, ix ) => g.map( ( _, ix + 1 ) ) }.toMap

}

object CustomGroupLists {
  def empty( groupCount: Int ): CustomGroupLists =
    CustomGroupLists( Vector.fill( groupCount )( Vector.empty[RecipeId] ) )

  def of( groupCount: Int, assignments: Map[RecipeId, Int] ): CustomGroupLists =
    empty( groupCount ).update( assignments )
}
