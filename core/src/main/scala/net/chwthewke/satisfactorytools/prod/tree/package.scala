package net.chwthewke.satisfactorytools
package prod

import cats.free.Cofree

package object tree {
  type Tree = Cofree[Vector, Vector[ClockedRecipe]]
}
