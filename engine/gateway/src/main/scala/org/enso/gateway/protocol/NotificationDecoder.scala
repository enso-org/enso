package org.enso.gateway.protocol

import io.circe.{ACursor, Decoder, DecodingFailure}
import org.enso.gateway.JsonRpcController.jsonRpcVersion
import org.enso.gateway.protocol.request.Params
import org.enso.gateway.protocol.request.Params.{InitializeParams, VoidParams}

/** Helper object for decoding [[Notification]]. */
object NotificationDecoder {

  /** Makes Circe decoder for notifications and notification fields of requests.
    *
    * @tparam P Subtype of [[Params]] for a notification with specific method.
    * @return the Circe decoder.
    */
  def instance[P <: Params]: Decoder[Notification[P]] =
    cursor => {
      val jsonrpcCursor = cursor.downField(Notification.jsonrpcField)
      val methodCursor  = cursor.downField(Notification.methodField)
      val paramsCursor  = cursor.downField(Notification.paramsField)
      val jsonrpcResult = validateJsonrpc(jsonrpcCursor)
      val methodResult  = Decoder[String].tryDecode(methodCursor)
      val paramsResult = methodResult
        .flatMap(selectParamsDecoder(_).tryDecode(paramsCursor))
      for {
        jsonrpc <- jsonrpcResult
        method  <- methodResult
        params  <- paramsResult
      } yield Notification[P](jsonrpc, method, params)
    }

  private def selectParamsDecoder[P <: Params](
    method: String
  ): Decoder[Option[P]] =
    (method match {
      case Requests.Initialize.method =>
        Decoder[Option[InitializeParams]]
      case Requests.Shutdown.method =>
        Decoder[Option[VoidParams]]

      case Notifications.Initialized.method | Notifications.Exit.method =>
        Decoder[Option[VoidParams]]

      case m =>
        Decoder.failed(
          RequestOrNotificationDecoder.unknownMethodFailure(m)
        )
    }).asInstanceOf[Decoder[Option[P]]]

  private def validateJsonrpc[P <: Params](
    jsonrpcCursor: ACursor
  ): Decoder.Result[String] = {
    Decoder[String].tryDecode(jsonrpcCursor).flatMap {
      case version @ `jsonRpcVersion` => Right(version)
      case version =>
        Left(
          wrongJsonRpcVersionFailure(version, jsonrpcCursor)
        )
    }
  }

  private def wrongJsonRpcVersionFailure(
    version: String,
    jsonrpcCursor: ACursor
  ): DecodingFailure =
    DecodingFailure(
      wrongJsonRpcVersionMessage(version),
      jsonrpcCursor.history
    )

  private def wrongJsonRpcVersionMessage(version: String) =
    s"jsonrpc must be $jsonRpcVersion but found $version"
}
