package org.enso.gateway.protocol

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder
import org.enso.gateway.JsonRpcController.jsonRpcVersion
import org.enso.gateway.protocol.response.{ResponseError, Result}

/** `ResponseMessage` in LSP Spec.
  *
  * https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#responseMessage
  *
  * @param jsonrpc JSON-RPC Version.
  * @param id      The request id.
  * @param result  The result of a request. This member is required on success
  *                and must not exist if there was an error.
  * @param error   The error object in case a request fails.
  */
case class Response private (
  jsonrpc: String,
  id: Option[Id],
  result: Option[Result],
  error: Option[ResponseError]
)
object Response {

  /** Creates response with a result.
    *
    * @param id     Id of request.
    * @param result [[Result]] of response.
    * @return the response.
    */
  def result(
    id: Option[Id] = None,
    result: Result
  ): Response =
    Response(jsonRpcVersion, id, Some(result), None)

  /** Creates response with an error.
    *
    * @param id    Id of request.
    * @param error [[ResponseError]] of response.
    * @return the response.
    */
  def error(
    id: Option[Id] = None,
    error: ResponseError
  ): Response =
    Response(jsonRpcVersion, id, None, Some(error))

  implicit def responseEncoder: Encoder[Response] = deriveEncoder
}
