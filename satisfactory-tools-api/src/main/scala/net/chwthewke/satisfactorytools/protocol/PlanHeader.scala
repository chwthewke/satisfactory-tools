package net.chwthewke.satisfactorytools
package protocol

import cats.Show
import cats.derived.semiauto
import cats.syntax.apply._
import io.chrisdavenport.cats.time.instances.instant._
import java.time.Instant

case class PlanHeader(
    id: PlanId,
    owner: UserId,
    title: Option[PlanName],
    copy: Option[PlanId],
    dirty: Boolean,
    updated: Instant,
    solution: SolutionHeader[SolutionId]
) {
  def isTransient: Boolean = title.isEmpty || copy.isDefined
}

object PlanHeader {
  type Row =
    (
        PlanId,
        UserId,
        Option[PlanName],
        Option[PlanName],
        Option[PlanId],
        Instant,
        Option[SolutionId],
        Option[String],
        Option[Int],
        Option[Int]
    )

  def apply( row: Row ): PlanHeader = row match {
    case (
        id,
        owner,
        nameOpt,
        srcNameOpt,
        srcIdOpt,
        updated,
        solutionIdOpt,
        solutionErrorOpt,
        groupCountOpt,
        lastGroupOpt
        ) =>
      PlanHeader(
        id,
        owner,
        nameOpt.orElse( srcNameOpt ),
        srcIdOpt,
        nameOpt.isEmpty,
        updated,
        ( solutionIdOpt, groupCountOpt )
          .mapN( SolutionHeader.Computed( _, _, lastGroupOpt.getOrElse( 0 ) ) )
          .orElse( solutionErrorOpt.map( SolutionHeader.PlanError ) )
          .getOrElse( SolutionHeader.NotComputed )
      )
  }

  def apply(
      id: PlanId,
      owner: UserId,
      nameOpt: Option[PlanName],
      srcNameOpt: Option[PlanName],
      srcIdOpt: Option[PlanId],
      updated: Instant,
      solutionIdOpt: Option[SolutionId],
      solutionErrorOpt: Option[String],
      groupCountOpt: Option[Int],
      lastGroupOpt: Option[Int]
  ): PlanHeader =
    apply(
      (
        id,
        owner,
        nameOpt,
        srcNameOpt,
        srcIdOpt,
        updated,
        solutionIdOpt,
        solutionErrorOpt,
        groupCountOpt,
        lastGroupOpt
      )
    )

  implicit val planHeaderShow: Show[PlanHeader] = semiauto.show[PlanHeader]
}
