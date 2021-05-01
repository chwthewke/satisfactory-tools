package net.chwthewke.satisfactorytools
package web.protocol

import scala.collection.Factory
import scodec.Codec
import scodec.bits.BitVector
import scodec.codecs

class SubsetCodecPartiallyApplied[CC[x] <: Iterable[x]] {

  def apply[A]( values: IndexedSeq[A] )( implicit F: Factory[A, CC[A]] ): Codec[CC[A]] = {
    def setToBitVector( subset: CC[A] ): BitVector = BitVector.bits( values.map( subset.toSet ) )
    def setOfBitVector( bits: BitVector ): CC[A] =
      values.zip( bits.toIndexedSeq ).filter( _._2 ).map( _._1 ).to( F )

    codecs.bits( values.size.toLong ).xmap( setOfBitVector, setToBitVector )
  }
}
