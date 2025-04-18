package org.enso.projectmanager.boot.configuration

import scala.concurrent.duration.FiniteDuration

/** A configuration object for supervisor properties.
  *
  * @param initialDelay a time that the supervisor wait before starts
  *                     monitoring
  * @param heartbeatInterval an interval between heartbeat sessions
  * @param heartbeatTimeout a timeout for pong reply
  * @param numberOfRestarts a maximum number of restarts
  * @param delayBetweenRestarts a delay between server restarts
  */
case class SupervisionConfig(
  initialDelay: FiniteDuration,
  heartbeatInterval: FiniteDuration,
  heartbeatTimeout: FiniteDuration,
  numberOfRestarts: Int,
  delayBetweenRestarts: FiniteDuration
)
