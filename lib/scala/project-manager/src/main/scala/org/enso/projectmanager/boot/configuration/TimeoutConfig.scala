package org.enso.projectmanager.boot.configuration

import scala.concurrent.duration.FiniteDuration

/** A configuration object for timeout properties.
  *
  * @param ioTimeout a timeout for IO operations
  * @param requestTimeout a timeout for JSON RPC request timeout
  * @param bootTimeout a timeout for booting process
  * @param shutdownTimeout a timeout for shutdown request
  * @param delayedShutdownTimeout a timeout when shutdown, caused by lack of clients, can be cancelled
  * @param socketCloseTimeout a timeout for closing the socket
  * @param retries a number of retries attempted when timeout is reached
  */
case class TimeoutConfig(
  ioTimeout: FiniteDuration,
  requestTimeout: FiniteDuration,
  bootTimeout: FiniteDuration,
  shutdownTimeout: FiniteDuration,
  delayedShutdownTimeout: FiniteDuration,
  socketCloseTimeout: FiniteDuration,
  retries: Int
)
