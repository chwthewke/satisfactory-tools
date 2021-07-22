package net.chwthewke.satisfactorytools
package protocol

import cats.Show
import cats.derived.semiauto
import cats.syntax.apply._
import io.chrisdavenport.cats.time.instances.instant._
import java.time.Instant

case class PlanHeader(
    title: Option[PlanName],
    copy: Option[PlanId],
    updated: Instant,
    solution: SolutionHeader
)

object PlanHeader {
  def apply(
      nameOpt: Option[PlanName],
      srcIdOpt: Option[PlanId],
      updated: Instant,
      solutionIdOpt: Option[SolutionId],
      solutionErrorOpt: Option[String],
      groupCountOpt: Option[Int],
      lastGroupOpt: Option[Int]
  ): PlanHeader =
    PlanHeader(
      nameOpt,
      srcIdOpt,
      updated,
      ( solutionIdOpt, groupCountOpt )
        .mapN( SolutionHeader.Computed( _, _, lastGroupOpt.getOrElse( 0 ) ) )
        .orElse( solutionErrorOpt.map( SolutionHeader.PlanError ) )
        .getOrElse( SolutionHeader.NotComputed )
    )

  implicit val planHeaderShow: Show[PlanHeader] = semiauto.show[PlanHeader]
}
