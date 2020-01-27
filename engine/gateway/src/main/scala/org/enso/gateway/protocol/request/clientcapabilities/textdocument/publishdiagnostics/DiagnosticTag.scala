package org.enso.gateway.protocol.request.clientcapabilities.textdocument.publishdiagnostics

import io.circe.Decoder

/** Element of [[TagSupport]]. */
sealed abstract class DiagnosticTag(value: Int)
object DiagnosticTag {
  private val unnecessary          = 1
  private val deprecated           = 2
  private val invalidDiagnosticTag = "Invalid DiagnosticTag"

  /** Unused or unnecessary code.
    *
    * Clients are allowed to render diagnostics with this tag faded out instead
    * of having an error squiggle.
    */
  case object Unnecessary extends DiagnosticTag(unnecessary)

  /** Deprecated or obsolete code.
    *
    * Clients are allowed to rendered diagnostics with this tag strike through.
    */
  case object Deprecated extends DiagnosticTag(deprecated)

  implicit val diagnosticTagDecoder: Decoder[DiagnosticTag] =
    Decoder.decodeInt.emap {
      case `unnecessary` => Right(Unnecessary)
      case `deprecated`  => Right(Deprecated)
      case _             => Left(invalidDiagnosticTag)
    }
}
