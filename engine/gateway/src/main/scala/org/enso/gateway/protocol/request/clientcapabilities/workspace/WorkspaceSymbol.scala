package org.enso.gateway.protocol.request.clientcapabilities.workspace

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/** Capabilities specific to the `workspace/symbol` request. */
case class WorkspaceSymbol()
object WorkspaceSymbol {
  implicit val clientCapabilitiesWorkspaceWorkspaceSymbolDecoder
    : Decoder[WorkspaceSymbol] =
    deriveDecoder
}
