package net.chwthewke.satisfactorytools
package data

import cats.Show
import cats.derived.semiauto
import io.circe.Decoder

case class LogisticsData(
    className: ClassName,
    displayName: String,
    amountPerMinute: Int
)

object LogisticsData {
  implicit val conveyorBeltDataShow: Show[LogisticsData] = semiauto.show[LogisticsData]

  private implicit val doubleDecoder: Decoder[Double] = Decoders.doubleStringDecoder

  case class ConveyorBelt( data: LogisticsData )
  object ConveyorBelt {
    def apply( className: ClassName, displayName: String, speed: Double ): ConveyorBelt =
      ConveyorBelt( LogisticsData( className, displayName, ( speed / 2 ).toInt ) )

    implicit val conveyorBeltDataDecoder: Decoder[ConveyorBelt] =
      Decoder.forProduct3( "ClassName", "mDisplayName", "mSpeed" )( ConveyorBelt.apply )
  }

  case class Pipeline( data: LogisticsData )
  object Pipeline {
    def apply( className: ClassName, displayName: String, flowLimit: Double ): Pipeline =
      Pipeline( LogisticsData( className, displayName, flowLimit.toInt * 60 ) )

    implicit val pipeDecoder: Decoder[Pipeline] =
      Decoder.forProduct3( "ClassName", "mDisplayName", "mFlowLimit" )( Pipeline.apply )
  }
}
