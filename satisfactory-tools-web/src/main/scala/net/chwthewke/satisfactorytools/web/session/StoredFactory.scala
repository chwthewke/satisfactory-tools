package net.chwthewke.satisfactorytools
package web.session

import java.time.Instant

import prod.Factory
import prod.SolverInputs

case class StoredFactory( lastModified: Instant, inputs: SolverInputs, solution: Factory )
