package org.enso.launcher.locking

/** Default [[ResourceManager]] using the [[DefaultFileLockManager]].
  */
object DefaultResourceManager extends ResourceManager(DefaultFileLockManager)
