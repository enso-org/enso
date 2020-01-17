package org.enso.gateway.protocol.response.result

import io.circe.generic.semiauto.deriveEncoder
import io.circe.Encoder
import org.enso.gateway.protocol.response.result.servercapabilities.{
  CodeActionProvider,
  CodeLensProvider,
  ColorProvider,
  CompletionOptions,
  DeclarationProvider,
  DefinitionProvider,
  DocumentFormattingProvider,
  DocumentHighlightProvider,
  DocumentLinkProvider,
  DocumentOnTypeFormattingProvider,
  DocumentRangeFormattingProvider,
  DocumentSymbolProvider,
  ExecuteCommandOptions,
  Experimental,
  FoldingRangeProvider,
  HoverProvider,
  ImplementationProvider,
  ReferencesProvider,
  RenameProvider,
  SignatureHelpOptions,
  TextDocumentSync,
  TypeDefinitionProvider,
  Workspace
}

/** [[org.enso.gateway.protocol.response.Result.InitializeResult]] server
  * capabilities.
  *
  * @param textDocumentSync                 @see [[TextDocumentSync]]
  * @param completionProvider               @see [[CompletionOptions]]
  * @param hoverProvider                    @see [[HoverProvider]]
  * @param signatureHelpProvider            @see [[SignatureHelpOptions]]
  * @param declarationProvider              @see [[DeclarationProvider]]
  * @param definitionProvider               @see [[DefinitionProvider]]
  * @param typeDefinitionProvider           @see [[TypeDefinitionProvider]]
  * @param implementationProvider           @see [[ImplementationProvider]]
  * @param referencesProvider               @see [[ReferencesProvider]]
  * @param documentHighlightProvider        @see [[DocumentHighlightProvider]]
  * @param documentSymbolProvider           @see [[DocumentSymbolProvider]]
  * @param codeActionProvider               @see [[CodeActionProvider]]
  * @param codeLensProvider                 @see [[CodeLensProvider]]
  * @param documentLinkProvider             @see [[DocumentLinkProvider]]
  * @param colorProvider                    @see [[ColorProvider]]
  * @param documentFormattingProvider       @see [[DocumentFormattingProvider]]
  * @param documentRangeFormattingProvider  @see
  *                                         [[DocumentRangeFormattingProvider]]
  * @param documentOnTypeFormattingProvider @see
  *                                         [[DocumentOnTypeFormattingProvider]]
  * @param renameProvider                   @see [[RenameProvider]]
  * @param foldingRangeProvider             @see [[FoldingRangeProvider]]
  * @param executeCommandProvider           @see [[ExecuteCommandOptions]]
  * @param workspaceSymbolProvider          The server provides workspace symbol
  *                                         support.
  * @param workspace                        @see [[Workspace]]
  * @param experimental                     @see [[Experimental]]
  */
case class ServerCapabilities(
  textDocumentSync: Option[TextDocumentSync]                     = None,
  completionProvider: Option[CompletionOptions]                  = None,
  hoverProvider: Option[HoverProvider]                           = None,
  signatureHelpProvider: Option[SignatureHelpOptions]            = None,
  declarationProvider: Option[DeclarationProvider]               = None,
  definitionProvider: Option[DefinitionProvider]                 = None,
  typeDefinitionProvider: Option[TypeDefinitionProvider]         = None,
  implementationProvider: Option[ImplementationProvider]         = None,
  referencesProvider: Option[ReferencesProvider]                 = None,
  documentHighlightProvider: Option[DocumentHighlightProvider]   = None,
  documentSymbolProvider: Option[DocumentSymbolProvider]         = None,
  codeActionProvider: Option[CodeActionProvider]                 = None,
  codeLensProvider: Option[CodeLensProvider]                     = None,
  documentLinkProvider: Option[DocumentLinkProvider]             = None,
  colorProvider: Option[ColorProvider]                           = None,
  documentFormattingProvider: Option[DocumentFormattingProvider] = None,
  documentRangeFormattingProvider: Option[DocumentRangeFormattingProvider] =
    None,
  documentOnTypeFormattingProvider: Option[DocumentOnTypeFormattingProvider] =
    None,
  renameProvider: Option[RenameProvider]                = None,
  foldingRangeProvider: Option[FoldingRangeProvider]    = None,
  executeCommandProvider: Option[ExecuteCommandOptions] = None,
  workspaceSymbolProvider: Option[Boolean]              = None,
  workspace: Option[Workspace]                          = None,
  experimental: Option[Experimental]                    = None
)
object ServerCapabilities {
  implicit val serverCapabilitiesEncoder: Encoder[ServerCapabilities] =
    deriveEncoder
}
