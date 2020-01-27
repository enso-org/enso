package org.enso.gateway.protocol

import org.enso.gateway.protocol.request.Params
import org.enso.gateway.protocol.request.Params.VoidParams

/** Parent class for notification extractor objects. */
sealed abstract class NotificationExtractor[T <: Params](
  val method: String
) {
  def unapply(
    request: Notification[T]
  ): Option[Option[T]] =
    request.method match {
      case `method` =>
        Some(request.params)
      case _ => None
    }
}

/** All notifications. */
object Notifications {

  /** Sent from the client to the server after the client received the result of
    * the initialize request but before the client is sending any other request
    * or notification to the server.
    *
    * LSP Spec:
    * https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#initialized
    */
  object Initialized extends NotificationExtractor[VoidParams]("initialized")

  /** Asks the server to exit its process.
    *
    * LSP Spec:
    * https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#exit
    */
  object Exit extends NotificationExtractor[VoidParams]("exit")

}
