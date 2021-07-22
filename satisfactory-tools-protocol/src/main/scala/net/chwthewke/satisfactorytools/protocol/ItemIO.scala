package net.chwthewke.satisfactorytools
package protocol

import data.Countable
import model.Recipe

case class ItemIO( sources: Vector[Countable[Double, Recipe]], destinations: Vector[Countable[Double, Recipe]] )
