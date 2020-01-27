package org.enso.gateway.protocol.request.clientcapabilities.workspace

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/** Capabilities specific to the `workspace/didChangeConfiguration`
  * notification.
  */
case class DidChangeConfiguration(dynamicRegistration: Option[Boolean] = None)
object DidChangeConfiguration {
  implicit val clientCapabilitiesWorkspaceDidChangeConfigurationDecoder
    : Decoder[DidChangeConfiguration] =
    deriveDecoder
}
