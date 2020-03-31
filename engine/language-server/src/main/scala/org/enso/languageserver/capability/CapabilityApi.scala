package org.enso.languageserver.capability

import org.enso.languageserver.data.CapabilityRegistration
import org.enso.jsonrpc.{Error, HasParams, HasResult, Method, Unused}

/**
  * The capability JSON RPC API provided by the language server.
  * See [[https://github.com/luna/enso/blob/master/doc/design/engine/engine-services.md]]
  * for message specifications.
  */
object CapabilityApi {

  case object AcquireCapability extends Method("capability/acquire") {
    implicit val hasParams = new HasParams[this.type] {
      type Params = CapabilityRegistration
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object ReleaseCapability extends Method("capability/release") {
    implicit val hasParams = new HasParams[this.type] {
      type Params = CapabilityRegistration
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object ForceReleaseCapability
      extends Method("capability/forceReleased") {
    implicit val hasParams = new HasParams[this.type] {
      type Params = CapabilityRegistration
    }
  }

  case object GrantCapability extends Method("capability/granted") {
    implicit val hasParams = new HasParams[this.type] {
      type Params = CapabilityRegistration
    }
  }

  // Errors

  case object CapabilityNotAcquired
      extends Error(5001, "Capability not acquired")
}
