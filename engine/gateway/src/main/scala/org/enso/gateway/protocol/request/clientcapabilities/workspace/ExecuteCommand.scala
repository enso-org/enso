package org.enso.gateway.protocol.request.clientcapabilities.workspace

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/** Capabilities specific to the `workspace/executeCommand` request. */
case class ExecuteCommand()
object ExecuteCommand {
  implicit val clientCapabilitiesWorkspaceExecuteCommandDecoder
    : Decoder[ExecuteCommand] =
    deriveDecoder
}
