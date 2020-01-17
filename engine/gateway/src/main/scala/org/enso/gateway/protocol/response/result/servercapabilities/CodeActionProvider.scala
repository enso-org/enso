package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

/** The server provides code actions. The `CodeActionOptions` return type is
  * only valid if the client signals code action literal support via the
  * property `textDocument.codeAction.codeActionLiteralSupport`.
  */
case class CodeActionProvider()
object CodeActionProvider {
  implicit val serverCapabilitiesCodeActionProviderEncoder
    : Encoder[CodeActionProvider] =
    deriveEncoder
}
