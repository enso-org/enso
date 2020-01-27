package org.enso.gateway.protocol.request.clientcapabilities

import io.circe.Decoder
import io.circe.generic.extras.semiauto.deriveUnwrappedDecoder

/** Defines capabilities for experimental features the client supports. */
case class Experimental(value: String) extends AnyVal
object Experimental {
  implicit val clientCapabilitiesExperimentalDecoder: Decoder[Experimental] =
    deriveUnwrappedDecoder
}
