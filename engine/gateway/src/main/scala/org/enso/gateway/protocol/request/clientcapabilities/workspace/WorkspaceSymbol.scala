package org.enso.gateway.protocol.request.clientcapabilities.workspace

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import org.enso.gateway.protocol.request.clientcapabilities.common.SymbolKinds

/** Capabilities specific to the `workspace/symbol` request. */
case class WorkspaceSymbol(
  dynamicRegistration: Option[Boolean] = None,
  symbolKind: Option[SymbolKinds]      = None
)
object WorkspaceSymbol {
  implicit val clientCapabilitiesWorkspaceWorkspaceSymbolDecoder
    : Decoder[WorkspaceSymbol] =
    deriveDecoder
}
