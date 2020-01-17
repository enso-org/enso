package org.enso.gateway.protocol.request.clientcapabilities.workspace

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/** Capabilities specific to the `workspace/didChangeWatchedFiles` notification.
  */
case class DidChangeWatchedFiles()
object DidChangeWatchedFiles {
  implicit val clientCapabilitiesWorkspaceDidChangeWatchedFilesDecoder
    : Decoder[DidChangeWatchedFiles] =
    deriveDecoder
}
