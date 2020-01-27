package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.generic.extras.semiauto.deriveUnwrappedEncoder

/** Experimental
  * [[org.enso.gateway.protocol.response.result.ServerCapabilities]].
  */
case class Experimental(value: String) extends AnyVal
object Experimental {
  implicit val serverCapabilitiesExperimentalEncoder: Encoder[Experimental] =
    deriveUnwrappedEncoder
}
