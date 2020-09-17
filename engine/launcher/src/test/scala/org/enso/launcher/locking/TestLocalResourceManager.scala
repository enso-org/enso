package org.enso.launcher.locking

object TestLocalResourceManager {

  /**
    * Creates a [[ResourceManager]] that manages resource access between threads
    * of a single process.
    *
    * The resource locks are not visible by other processes, so this manager is
    * not useful for synchronizing multiple processes. It can be used to test
    * concurrency implementation using threads within the same JVM, in contrary
    * to the default [[FileLockManager]] which can be only used for
    * inter-process synchronization.
    */
  def create(): ResourceManager = new ResourceManager(new TestLocalLockManager)
}
