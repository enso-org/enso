package org.enso.languageserver.capability

import org.enso.languageserver.data.CapabilityRegistration
import org.enso.languageserver.filemanager.FileSystemFailure
import org.enso.languageserver.session.JsonSession

object CapabilityProtocol {

  /**
    * Requests the Language Server grant a new capability to a client.
    *
    * @param rpcSession the client to grant the capability to.
    * @param registration the capability to grant.
    */
  case class AcquireCapability(
    rpcSession: JsonSession,
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
    * Signals about capability acquisition error.
    *
    * @param error file system failure
    */
  case class CapabilityAcquisitionFileSystemFailure(error: FileSystemFailure)
      extends AcquireCapabilityResponse

  /**
    * Notifies the Language Server about a client releasing a capability.
    *
    * @param rpcSession the client releasing the capability.
    * @param capability the capability being released.
    */
  case class ReleaseCapability(
    rpcSession: JsonSession,
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
    * Signals that requested capability is not acquired.
    */
  case object CapabilityNotAcquiredResponse extends ReleaseCapabilityResponse

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
