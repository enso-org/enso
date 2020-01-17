package org.enso.gateway.protocol.request.clientcapabilities.workspace

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/** Capabilities specific to `WorkspaceEdit`s. */
case class Edit()
object Edit {
  implicit val clientCapabilitiesWorkspaceEditDecoder: Decoder[Edit] =
    deriveDecoder
}
