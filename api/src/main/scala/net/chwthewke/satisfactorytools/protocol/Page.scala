package net.chwthewke.satisfactorytools
package protocol

case class Page[A](
    items: Vector[A],
    query: PageQuery,
    hasPreviousPage: Boolean,
    hasNextPage: Boolean
)
