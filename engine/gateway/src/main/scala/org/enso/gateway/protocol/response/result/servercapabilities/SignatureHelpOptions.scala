package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

/** Server capability to provide signature help support. */
case class SignatureHelpOptions(
  triggerCharacters: Option[Seq[String]]   = None,
  retriggerCharacters: Option[Seq[String]] = None,
  workDoneProgress: Option[Boolean]        = None
)
object SignatureHelpOptions {
  implicit val serverCapabilitiesSignatureHelpOptionsEncoder
    : Encoder[SignatureHelpOptions] =
    deriveEncoder
}
