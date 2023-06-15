package net.chwthewke.satisfactorytools
package web.view

import scalatags.Text.Tag

import prod.tree.FactoryTree

object TreeZoomView extends ( ( FactoryTree.At, Int ) => Tag ) {
  override def apply( treeAt: FactoryTree.At, groupCount: Int ): Tag = ???
}
