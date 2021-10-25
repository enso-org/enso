package org.enso.languageserver.util.binary

import java.nio.ByteBuffer

/** A type class that provides a way to encode a value of type `A` to bytes.
  *
  * @tparam A an input type
  */
trait BinaryEncoder[-A] {

  /** Encodes a value of type 'A' to bytes.
    *
    * @param msg a message to encode
    * @return a byte buffer containing serialized format of type `A`
    */
  def encode(msg: A): ByteBuffer

}

object BinaryEncoder {

  /** An encoder that returns input value.
    */
  val empty: BinaryEncoder[ByteBuffer] = identity

}
