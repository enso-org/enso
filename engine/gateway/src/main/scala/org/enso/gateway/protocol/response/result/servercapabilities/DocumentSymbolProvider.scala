package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.syntax._
import io.circe.generic.extras.semiauto.deriveUnwrappedEncoder
import io.circe.generic.semiauto.deriveEncoder

/** Server capability to provide document symbol support. */
sealed trait DocumentSymbolProvider
object DocumentSymbolProvider {

  case class Bool(value: Boolean) extends DocumentSymbolProvider
  object Bool {
    implicit val boolEncoder: Encoder[Bool] =
      deriveUnwrappedEncoder
  }

  case class DocumentSymbolOptions(workDoneProgress: Option[Boolean] = None)
      extends DocumentSymbolProvider
  object DocumentSymbolOptions {
    implicit val documentSymbolOptionsEncoder: Encoder[DocumentSymbolOptions] =
      deriveEncoder
  }

  implicit val serverCapabilitiesDocumentSymbolProviderEncoder
    : Encoder[DocumentSymbolProvider] =
    Encoder.instance {
      case boolean: Bool                  => boolean.asJson
      case options: DocumentSymbolOptions => options.asJson
    }
}
