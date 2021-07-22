package net.chwthewke.satisfactorytools
package protocol

case class PlanHeader( title: Option[String], dirty: Boolean, customGroupCount: Int, lastCustomGroup: Int )
