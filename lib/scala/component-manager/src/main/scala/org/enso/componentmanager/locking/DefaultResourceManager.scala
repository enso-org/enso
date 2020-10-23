package org.enso.componentmanager.locking

/** Default [[ResourceManager]] using the [[DefaultFileLockManager]].
  */
object DefaultResourceManager extends ResourceManager(DefaultFileLockManager)
