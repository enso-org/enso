package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

/** The server provides folding provider support. */
case class FoldingRangeProvider()
object FoldingRangeProvider {
  implicit val serverCapabilitiesFoldingRangeProviderEncoder
    : Encoder[FoldingRangeProvider] =
    deriveEncoder
}
