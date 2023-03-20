package net.chwthewke.satisfactorytools
package protocol

import cats.Show
import cats.derived.semiauto
import cats.syntax.apply._
import java.time.Instant
import org.typelevel.cats.time.instances.instant._

case class PlanHeader(
    id: PlanId,
    modelVersionId: ModelVersionId,
    owner: UserId,
    title: Option[PlanName],
    srcId: Option[PlanId],
    dirty: Boolean,
    updated: Instant,
    solution: SolutionHeader[SolutionId]
) {
  def isTransient: Boolean = title.isEmpty || srcId.isDefined
}

object PlanHeader {
  type Row =
    (
        PlanId,
        ModelVersionId,
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
        modelVersionId,
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
        modelVersionId,
        owner,
        nameOpt.orElse( srcNameOpt ),
        srcIdOpt,
        nameOpt.isEmpty,
        updated,
        solutionErrorOpt
          .map( SolutionHeader.PlanError )
          .orElse(
            ( solutionIdOpt, groupCountOpt )
              .mapN( SolutionHeader.Computed( _, _, lastGroupOpt.getOrElse( 0 ) ) )
          )
          .getOrElse( SolutionHeader.NotComputed )
      )
  }

  implicit val planHeaderShow: Show[PlanHeader] = semiauto.show[PlanHeader]
}
