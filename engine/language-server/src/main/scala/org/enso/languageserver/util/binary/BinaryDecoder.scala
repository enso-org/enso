package org.enso.languageserver.util.binary

import java.nio.ByteBuffer

/**
  * A type class that provides a way to decode bytes to value of type `A`.
  *
  * @tparam A a result type
  */
trait BinaryDecoder[+A] {

  /**
    * Decodes serialized format of type `A`.
    *
    * @param bytes a byte buffer containing serialized format of type `A`
    * @return either a decoding failure or a value of type `A`
    */
  def decode(bytes: ByteBuffer): Either[DecodingFailure, A]

}
