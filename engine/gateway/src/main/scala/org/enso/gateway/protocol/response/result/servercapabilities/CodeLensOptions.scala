package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

/** Server capability to provide code lens. */
case class CodeLensOptions(
  workDoneProgress: Option[Boolean] = None,
  resolveProvider: Option[Boolean]  = None
)
object CodeLensOptions {
  implicit val serverCapabilitiesCodeLensOptionsEncoder
    : Encoder[CodeLensOptions] =
    deriveEncoder
}
