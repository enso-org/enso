package org.enso.gateway.protocol

import io.circe.{Decoder, Encoder}

/** Kind of
  * [[org.enso.gateway.protocol.request.clientcapabilities.textdocument.CodeAction]].
  *
  * Used also in
  * [[org.enso.gateway.protocol.response.result.servercapabilities.CodeActionProvider]].
  */
sealed abstract class CodeActionKind(val value: String)
object CodeActionKind {
  private val empty                 = ""
  private val quickfix              = "quickfix"
  private val refactor              = "refactor"
  private val refactorExtract       = "refactor.extract"
  private val refactorInline        = "refactor.inline"
  private val refactorRewrite       = "refactor.rewrite"
  private val source                = "source"
  private val sourceOrganizeImports = "source.organizeImports"
  private val invalidCodeActionKind = "Invalid CodeActionKind"

  /** Empty kind. */
  case object Empty extends CodeActionKind(empty)

  /** Base kind for quickfix actions. */
  case object QuickFix extends CodeActionKind(quickfix)

  /** Base kind for refactoring actions. */
  case object Refactor extends CodeActionKind(refactor)

  /** Base kind for refactoring extraction actions. */
  case object RefactorExtract extends CodeActionKind(refactorExtract)

  /** Base kind for refactoring inline actions. */
  case object RefactorInline extends CodeActionKind(refactorInline)

  /** Base kind for refactoring rewrite actions. */
  case object RefactorRewrite extends CodeActionKind(refactorRewrite)

  /** Base kind for source actions. Source code actions apply to the entire
    * file.
    */
  case object Source extends CodeActionKind(source)

  /** Base kind for an organize imports source action. */
  case object SourceOrganizeImports
      extends CodeActionKind(sourceOrganizeImports)

  implicit val codeActionKindDecoder: Decoder[CodeActionKind] =
    Decoder.decodeString.emap {
      case `empty`                 => Right(Empty)
      case `quickfix`              => Right(QuickFix)
      case `refactor`              => Right(Refactor)
      case `refactorExtract`       => Right(RefactorExtract)
      case `refactorInline`        => Right(RefactorInline)
      case `refactorRewrite`       => Right(RefactorRewrite)
      case `source`                => Right(Source)
      case `sourceOrganizeImports` => Right(SourceOrganizeImports)
      case _                       => Left(invalidCodeActionKind)
    }

  implicit val codeActionKindEncoder: Encoder[CodeActionKind] =
    Encoder.encodeString.contramap(_.value)
}
