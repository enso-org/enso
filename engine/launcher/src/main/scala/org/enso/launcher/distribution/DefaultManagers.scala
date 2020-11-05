package org.enso.launcher.distribution

import org.enso.runtimeversionmanager.distribution.{
  PortableDistributionManager,
  TemporaryDirectoryManager
}
import org.enso.runtimeversionmanager.locking.{
  ResourceManager,
  ThreadSafeFileLockManager
}

/** Gathers default managers used in the launcher. */
object DefaultManagers {

  /** Default distribution manager that is capable of detecting portable mode. */
  val distributionManager = new PortableDistributionManager(LauncherEnvironment)

  /** Default [[LockManager]] storing lock files in a directory defined by
    * the distribution manager.
    *
    * It is lazily initialized, because initializing it triggers path detection
    * and this cannot happen within static initialization, because with Native
    * Image, static initialization is done at build-time, so the paths would be
    * set at build time and not actual runtime, leading to very wrong results.
    */
  lazy val defaultFileLockManager =
    new ThreadSafeFileLockManager(distributionManager.paths.locks)

  /** Default [[ResourceManager]] using the [[defaultFileLockManager]]. */
  lazy val defaultResourceManager = new ResourceManager(defaultFileLockManager)

  /** Default [[TemporaryDirectoryManager]]. */
  lazy val temporaryDirectoryManager =
    new TemporaryDirectoryManager(distributionManager, defaultResourceManager)
}
