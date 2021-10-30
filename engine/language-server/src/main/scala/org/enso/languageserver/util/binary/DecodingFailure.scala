package org.enso.languageserver.util.binary

/** Base trait signaling decoding failure.
  */
sealed trait DecodingFailure

object DecodingFailure {

  /** Signals an empty payload in the byte stream.
    */
  case object EmptyPayload extends DecodingFailure

  /** Signals that data is corrupted.
    */
  case object DataCorrupted extends DecodingFailure

  /** Represents an undefined decoding failure.
    *
    * @param throwable a throwable
    */
  case class GenericDecodingFailure(throwable: Throwable)
      extends DecodingFailure

}
