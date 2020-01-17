package org.enso.gateway.protocol.response

import io.circe.Encoder
import org.enso.gateway.protocol.response.error.{Data, ErrorCode, ErrorMessage}
import org.enso.gateway.protocol.response.error.Data.{InitializeData, ParseData}

/** [[org.enso.gateway.protocol.Response]] error.
  *
  * `ResponseError` in LSP Spec:
  * https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#responseMessage
  */
sealed abstract class ResponseError(
  val code: ErrorCode,
  val message: String,
  val data: Option[Data]
)
object ResponseError {
  implicit val responseErrorEncoder: Encoder[ResponseError] = {
    val codeField    = "code"
    val messageField = "message"
    val dataField    = "data"
    Encoder.forProduct3(codeField, messageField, dataField)(
      error => (error.code, error.message, error.data)
    )
  }

  /** Invalid JSON. */
  case class ParseError(
    override val data: Option[ParseData] = None
  ) extends ResponseError(
        ErrorCode.ParseError,
        ErrorMessage.invalidJson,
        data
      )

  /** Unknown JSON-RPC method. */
  case class MethodNotFoundError(
    override val data: Option[Data] = None
  ) extends ResponseError(
        ErrorCode.MethodNotFound,
        ErrorMessage.methodNotFound,
        data
      )

  /** [[org.enso.gateway.protocol.Requests.Initialize]] error.
    * Wrong JSON-RPC version.
    */
  case class InitializeError(
    override val data: Option[InitializeData] = None
  ) extends ResponseError(
        ErrorCode.UnknownProtocolVersion,
        ErrorMessage.wrongJsonRpcVersion,
        data
      )

  /** Default type of errors. */
  case class UnexpectedError(
    override val data: Option[Data.Text] = None
  ) extends ResponseError(
        ErrorCode.UnknownErrorCode,
        ErrorMessage.unexpectedError,
        data
      )
}
