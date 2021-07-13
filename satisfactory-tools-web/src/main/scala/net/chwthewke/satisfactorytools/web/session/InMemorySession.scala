package net.chwthewke.satisfactorytools
package web.session

case class InMemorySession(
    history: List[StoredFactory],
    saved: Map[String, StoredFactory]
)

object InMemorySession {
  val empty: InMemorySession = InMemorySession( Nil, Map.empty )
}
