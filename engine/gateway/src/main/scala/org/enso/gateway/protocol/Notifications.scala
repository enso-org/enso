package org.enso.gateway.protocol

import org.enso.gateway.protocol.request.Params

/** Parent trait for notifications extractor objects. */
sealed trait Notifications {
  val method: String

  def unapply[T <: Params](
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

  /** LSP Spec:
    * https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#initialized
    */
  object Initialized extends Notifications {
    override val method = "initialized"
  }
}
