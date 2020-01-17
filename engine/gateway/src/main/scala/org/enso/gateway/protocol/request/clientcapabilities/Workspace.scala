package org.enso.gateway.protocol.request.clientcapabilities

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import org.enso.gateway.protocol.request.clientcapabilities.workspace.{
  DidChangeConfiguration,
  DidChangeWatchedFiles,
  Edit,
  ExecuteCommand,
  WorkspaceSymbol
}

/** Define capabilities for workspace features the client supports.
  *
  * @param applyEdit              The client supports applying batch edits to
  *                               the workspace by supporting the request
  *                               'workspace/applyEdit'.
  * @param workspaceEdit          @see [[Edit]]
  * @param didChangeConfiguration @see [[DidChangeConfiguration]]
  * @param didChangeWatchedFiles  @see [[DidChangeWatchedFiles]]
  * @param symbol                 @see [[WorkspaceSymbol]]
  * @param executeCommand         @see [[ExecuteCommand]]
  */
case class Workspace(
  applyEdit: Option[Boolean]                             = None,
  workspaceEdit: Option[Edit]                            = None,
  didChangeConfiguration: Option[DidChangeConfiguration] = None,
  didChangeWatchedFiles: Option[DidChangeWatchedFiles]   = None,
  symbol: Option[WorkspaceSymbol]                        = None,
  executeCommand: Option[ExecuteCommand]                 = None
)
object Workspace {
  implicit val clientCapabilitiesWorkspaceDecoder: Decoder[Workspace] =
    deriveDecoder
}
