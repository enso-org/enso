package org.enso.projectmanager.boot.configuration

/** A configuration object for networking.
  *
  * @param interface an interface to listen to
  * @param minPort min port for the LS
  * @param maxPort max port for the LS
  * @param enableSecure true, if secure connections should be enabled, false otherwise
  */
case class NetworkConfig(
  interface: String,
  minPort: Int,
  maxPort: Int,
  enableSecure: Boolean
)
