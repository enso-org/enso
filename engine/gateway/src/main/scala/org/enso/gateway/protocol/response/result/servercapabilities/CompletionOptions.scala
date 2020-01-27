package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

/** Server capability to provide completion support. */
case class CompletionOptions(
  triggerCharacters: Option[Seq[String]]   = None,
  allCommitCharacters: Option[Seq[String]] = None,
  resolveProvider: Option[Boolean]         = None,
  workDoneProgress: Option[Boolean]        = None
)
object CompletionOptions {
  implicit val serverCapabilitiesCompletionOptionsEncoder
    : Encoder[CompletionOptions] =
    deriveEncoder
}
