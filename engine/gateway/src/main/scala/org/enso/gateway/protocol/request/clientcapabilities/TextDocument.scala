package org.enso.gateway.protocol.request.clientcapabilities

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import org.enso.gateway.protocol.request.clientcapabilities.textdocument.{
  CodeAction,
  CodeLens,
  Color,
  Completion,
  Declaration,
  Definition,
  DocumentSymbol,
  FoldingRange,
  Formatting,
  Highlight,
  Hover,
  Implementation,
  Link,
  OnTypeFormatting,
  PublishDiagnostics,
  RangeFormatting,
  Reference,
  Rename,
  SignatureHelp,
  Sync,
  TypeDefinition
}

/** Define capabilities for text document features the client supports. */
case class TextDocument(
  synchronization: Option[Sync]                  = None,
  completion: Option[Completion]                 = None,
  hover: Option[Hover]                           = None,
  signatureHelp: Option[SignatureHelp]           = None,
  declaration: Option[Declaration]               = None,
  definition: Option[Definition]                 = None,
  typeDefinition: Option[TypeDefinition]         = None,
  implementation: Option[Implementation]         = None,
  references: Option[Reference]                  = None,
  documentHighlight: Option[Highlight]           = None,
  documentSymbol: Option[DocumentSymbol]         = None,
  codeAction: Option[CodeAction]                 = None,
  codeLens: Option[CodeLens]                     = None,
  documentLink: Option[Link]                     = None,
  colorProvider: Option[Color]                   = None,
  formatting: Option[Formatting]                 = None,
  rangeFormatting: Option[RangeFormatting]       = None,
  onTypeFormatting: Option[OnTypeFormatting]     = None,
  rename: Option[Rename]                         = None,
  publishDiagnostics: Option[PublishDiagnostics] = None,
  foldingRange: Option[FoldingRange]             = None
)
object TextDocument {
  implicit val clientCapabilitiesTextDocumentDecoder: Decoder[TextDocument] =
    deriveDecoder
}
