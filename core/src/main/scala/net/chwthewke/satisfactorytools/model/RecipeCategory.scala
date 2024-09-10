package net.chwthewke.satisfactorytools.model

import cats.Order
import cats.Show
import cats.derived.semiauto
import cats.syntax.apply._
import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

sealed trait RecipeCategory {
  def tierOpt: Option[Int]
  def typeOpt: Option[String]
}

object RecipeCategory {
  private val catMilestone: String    = "hub"
  private val catAlternate: String    = "alt"
  private val catNuclearWaste: String = "nuc"

  def of( tier: Int, `type`: String ): Either[String, RecipeCategory.Manufacturing] =
    ( `type` match {
      case `catMilestone`    => Some( Milestone( tier ) )
      case `catAlternate`    => Some( Alternate( tier ) )
      case `catNuclearWaste` => Some( NuclearWaste )
      case catResearch       => ResearchCategory.withNameOption( catResearch ).map( Mam( tier, _ ) )
    } ).toRight( s"Unknown recipe category type ${`type`}" )

  def of( tierOpt: Option[Int], typeOpt: Option[String] ): Either[String, RecipeCategory] =
    ( tierOpt, typeOpt ).traverseN( of ).map( _.getOrElse( RecipeCategory.Extraction ) )

  sealed trait Manufacturing extends RecipeCategory {
    def tier: Int
    def `type`: String

    override def tierOpt: Option[Int]    = Some( tier )
    override def typeOpt: Option[String] = Some( `type` )
  }

  case object Extraction extends RecipeCategory {
    override def tierOpt: Option[Int]    = None
    override def typeOpt: Option[String] = None
  }

  case object NuclearWaste extends RecipeCategory.Manufacturing {
    override def tier: Int      = 8
    override def `type`: String = catNuclearWaste
  }

  case class Milestone( override val tier: Int ) extends RecipeCategory.Manufacturing {
    override def `type`: String = catMilestone
  }
  case class Alternate( override val tier: Int ) extends RecipeCategory.Manufacturing {
    override def `type`: String = "alt"
  }
  case class Mam( override val tier: Int, researchCategory: ResearchCategory ) extends RecipeCategory.Manufacturing {
    override def `type`: String = researchCategory.entryName
  }

  implicit val recipeCategoryShow: Show[RecipeCategory] = semiauto.show[RecipeCategory]
  implicit val recipeCategoryOrder: Order[RecipeCategory] = Order.by {
    case Extraction                    => ( 0, 0, None )
    case Milestone( tier )             => ( tier, 1, None )
    case Mam( tier, researchCategory ) => ( tier, 2, Some( researchCategory ) )
    case Alternate( tier )             => ( tier, 3, None )
    case NuclearWaste                  => ( 0, 4, None )
  }
  implicit val recipeCategoryOrdering: Ordering[RecipeCategory] = Order.catsKernelOrderingForOrder

  implicit val recipeCategoryDecoder: Decoder[RecipeCategory] = deriveDecoder[RecipeCategory]
  implicit val recipeCategoryEncoder: Encoder[RecipeCategory] = deriveEncoder[RecipeCategory]
}
