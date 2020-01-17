package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

/** The server provides completion support. */
case class CompletionOptions()
object CompletionOptions {
  implicit val serverCapabilitiesCompletionOptionsEncoder
    : Encoder[CompletionOptions] =
    deriveEncoder
}
