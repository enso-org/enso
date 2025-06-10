package org.enso.projectmanager.boot.configuration

import scala.concurrent.duration.FiniteDuration

/** A configuration object for bootloader properties.
  *
  * @param numberOfRetries how many times a bootloader should try to boot the LS
  * @param delayBetweenRetry delays between retries
  * @param skipGraalVMUpdater indicates if the check and installation of the required GraalVM should be skipped
  */
case class BootloaderConfig(
  numberOfRetries: Int,
  delayBetweenRetry: FiniteDuration,
  skipGraalVMUpdater: Boolean = false
)
