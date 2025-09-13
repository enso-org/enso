package org.enso.runtimeversionmanager.test

import org.enso.distribution.locking.ResourceManager

object TestLocalResourceManager {

  /** Creates a [[ResourceManager]] that manages resource access between threads
    * of a single process.
    *
    * The resource locks are not visible by other processes, so this manager is
    * not useful for synchronizing multiple processes. It can be used to test
    * concurrency implementation using threads within the same JVM, in contrary
    * to the default [[FileLockManager]] which can be only used for
    * inter-process synchronization.
    *
    * The [[ResourceManager]] created using that method uses an independent lock
    * manager that is not shared with other resource managers. It is meant to be
    * used for non-concurrent tests. In concurrent tests, each used
    * [[ResourceManager]] should share their [[TestLocalLockManager]] so that
    * all the threads see each other's locks.
    */
  def create(): ResourceManager = new ResourceManager(new TestLocalLockManager)
}
