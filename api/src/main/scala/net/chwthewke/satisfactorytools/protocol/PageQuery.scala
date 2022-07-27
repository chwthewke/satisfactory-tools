package net.chwthewke.satisfactorytools
package protocol

case class PageQuery( pageNum: Int, pageSize: Int ) {
  def limit: Long  = pageSize.toLong + 1
  def offset: Long = (pageNum - 1).toLong * pageSize.toLong
}
