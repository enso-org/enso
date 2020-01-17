package org.enso.gateway.protocol

import org.enso.gateway.protocol.request.Params

/** Parent trait for requests extractor objects. */
sealed trait Requests {
  val method: String

  def unapply[T <: Params](
    request: Request[T]
  ): Option[(Id, Option[T])] =
    request.method match {
      case `method` =>
        Some((request.id, request.params))
      case _ => None
    }
}

/** All requests. */
object Requests {

  /** LSP Spec:
    * https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#initialize
    */
  object Initialize extends Requests {
    override val method = "initialize"
  }

}
