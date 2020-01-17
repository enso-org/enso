package org.enso.gateway.protocol

import io.circe.Decoder
import org.enso.gateway.protocol.request.Params

/**
  * Parent trait for [[Request]] and [[Notification]]
  */
sealed trait RequestOrNotification {

  /** JSON-RPC Version.
    *
    * @see [[org.enso.gateway.JsonRpcController.jsonRpcVersion]].
    */
  def jsonrpc: String

  /** The JSON-RPC method to be invoked. */
  def method: String
}
object RequestOrNotification {
  implicit val requestOrNotificationDecoder: Decoder[RequestOrNotification] =
    RequestOrNotificationDecoder.instance
}

/** `RequestMessage` in LSP Spec:
  * https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#requestMessage
  *
  * @param jsonrpc JSON-RPC Version
  * @param id      The request id
  * @param method  The JSON-RPC method to be invoked
  * @param params  The method's params. A Structured value that holds the parameter values
  *                to be used during the invocation of the method
  * @tparam P Subtype of [[Params]] for a request with specific method
  */
case class Request[P <: Params](
  jsonrpc: String,
  id: Id,
  method: String,
  params: Option[P]
) extends RequestOrNotification

object Request {

  /** Field `id`. */
  val idField = "id"

  implicit def requestDecoder[T <: Params]: Decoder[Request[T]] =
    RequestDecoder.instance
}

/** `NotificationMessage` in LSP Spec:
  * https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#notificationMessage
  * A processed notification message must not send a response back (they work
  * like events). Therefore no `id`.
  *
  * @param jsonrpc JSON-RPC Version.
  * @param method  The JSON-RPC method to be invoked.
  * @param params  The method's params. A structured value that holds the
  *                parameter values to be used during the invocation of the
  *                method.
  * @tparam P Subtype of [[Params]] for a notification with specific method.
  */
case class Notification[P <: Params](
  jsonrpc: String,
  method: String,
  params: Option[P]
) extends RequestOrNotification
object Notification {

  /** Field `jsonrpc` to be validated. */
  val jsonrpcField = "jsonrpc"

  /** Field `method`, which is discriminator. */
  val methodField = "method"

  /** Field `params`. */
  val paramsField = "params"

  implicit def notificationDecoder[P <: Params]: Decoder[Notification[P]] =
    NotificationDecoder.instance
}
