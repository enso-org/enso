package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.syntax._
import io.circe.generic.extras.semiauto.deriveUnwrappedEncoder
import io.circe.generic.semiauto.deriveEncoder
import org.enso.gateway.protocol.CodeActionKind

/** Server capability to provide code actions.
  *
  * The [[CodeActionProvider.CodeActionOptions]] return type is only valid if
  * the client signals code action literal support via the property
  * `textDocument.codeAction.codeActionLiteralSupport`.
  */
sealed trait CodeActionProvider
object CodeActionProvider {

  case class Bool(value: Boolean) extends CodeActionProvider
  object Bool {
    implicit val boolEncoder: Encoder[Bool] =
      deriveUnwrappedEncoder
  }

  case class CodeActionOptions(
    workDoneProgress: Option[Boolean]            = None,
    codeActionKinds: Option[Seq[CodeActionKind]] = None
  ) extends CodeActionProvider
  object CodeActionOptions {
    implicit val codeActionOptionsEncoder: Encoder[CodeActionOptions] =
      deriveEncoder
  }

  implicit val serverCapabilitiesCodeActionProviderEncoder
    : Encoder[CodeActionProvider] =
    Encoder.instance {
      case boolean: Bool              => boolean.asJson
      case options: CodeActionOptions => options.asJson
    }
}
