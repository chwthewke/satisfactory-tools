package net.chwthewke.satisfactorytools.model

import io.circe.Decoder

final case class Descriptions[A]( className: NativeClass, items: Map[ClassName, A] )

object Descriptions {
  def descriptionsDecoder[A](
      nativeClass: NativeClass,
      name: A => ClassName
  )( implicit D: Decoder[A] ): Decoder[Descriptions[A]] =
    Decoder[Vector[( ClassName, A )]].map( _.toMap ).map( Descriptions( nativeClass, _ ) )
}
