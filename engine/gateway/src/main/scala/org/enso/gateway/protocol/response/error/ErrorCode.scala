package org.enso.gateway.protocol.response.error

import io.circe.Encoder

/** Code of [[org.enso.gateway.protocol.response.ResponseError]]. */
sealed abstract class ErrorCode(val code: Int)
object ErrorCode {

  /** Signals that invalid JSON was received by the server.
    *
    * An error occurred on the server while parsing the JSON text.
    * Defined by JSON-RPC Spec.
    */
  case object ParseError extends ErrorCode(-32700)

  /** Signals that the JSON sent is not a valid Request object.
    *
    * Defined by JSON-RPC Spec.
    */
  case object InvalidRequest extends ErrorCode(-32600)

  /** Signals that the method does not exist or is not available.
    *
    * Defined by JSON-RPC Spec.
    */
  case object MethodNotFound extends ErrorCode(-32601)

  /** Signals that method parameters are invalid.
    *
    * Defined by JSON-RPC Spec.
    */
  case object InvalidParams extends ErrorCode(-32602)

  /** Internal JSON-RPC error.
    *
    * Defined by JSON-RPC Spec.
    */
  case object InternalError extends ErrorCode(-32603)

  /** Codes from -32000 to -32099 reserved for implementation-defined
    * server-errors.
    */
  case object ServerErrorEnd extends ErrorCode(-32000)

  case object UnknownErrorCode extends ErrorCode(-32001)

  case object ServerNotInitialized extends ErrorCode(-32002)

  case object ServerErrorStart extends ErrorCode(-32099)

  /** Error codes defined by LSP. */
  case object RequestCancelled extends ErrorCode(-32800)

  case object ContentModified extends ErrorCode(-32801)

  /** Error code for `initialize` method. */
  case object UnknownProtocolVersion extends ErrorCode(1)

  implicit val errorCodeEncoder: Encoder[ErrorCode] =
    Encoder.encodeInt.contramap(_.code)
}
