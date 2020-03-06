package org.enso.languageserver.capability

import org.enso.languageserver.data.{CapabilityRegistration, Client}

object CapabilityProtocol {

  /**
    * Requests the Language Server grant a new capability to a client.
    *
    * @param client the client to grant the capability to.
    * @param registration the capability to grant.
    */
  case class AcquireCapability(
    client: Client,
    registration: CapabilityRegistration
  )

  /**
    * Signals capability acquisition status.
    */
  sealed trait AcquireCapabilityResponse

  /**
    * Confirms client that capability has been acquired.
    */
  case object CapabilityAcquired extends AcquireCapabilityResponse

  /**
    * Signals that capability acquisition request cannot be processed.
    */
  case object CapabilityAcquisitionBadRequest extends AcquireCapabilityResponse

  /**
    * Notifies the Language Server about a client releasing a capability.
    *
    * @param clientId the client releasing the capability.
    * @param capability the capability being released.
    */
  case class ReleaseCapability(
    clientId: Client.Id,
    capability: CapabilityRegistration
  )

  /**
    * Signals capability release status.
    */
  sealed trait ReleaseCapabilityResponse

  /**
    * Confirms client that capability has been released.
    */
  case object CapabilityReleased extends ReleaseCapabilityResponse

  /**
    * Signals that capability release request cannot be processed.
    */
  case object CapabilityReleaseBadRequest extends ReleaseCapabilityResponse

  /**
    * A notification sent by the Language Server, notifying a client about
    * a capability being taken away from them.
    *
    * @param capability the capability being released.
    */
  case class CapabilityForceReleased(capability: CapabilityRegistration)

  /**
    * A notification sent by the Language Server, notifying a client about a new
    * capability being granted to them.
    *
    * @param registration the capability being granted.
    */
  case class CapabilityGranted(registration: CapabilityRegistration)

}
