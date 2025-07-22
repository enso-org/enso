package org.enso.runtimeversionmanager.test

import org.enso.distribution.locking.ThreadSafeFileLockManager

import java.nio.file.Path

class TestableThreadSafeFileLockManager(locksRoot: Path)
    extends ThreadSafeFileLockManager(locksRoot) {

  /** A helper function that can be called by the test suite to release all file locks.
    *
    * It is only safe to call this function if it can be guaranteed that no
    * locks created with this manager are still in scope.
    *
    * Normally all file locks are released automatically when the JVM exits,
    * but as tests run within a single JVM, this is not the case and any
    * dangling locks will cause problems when cleaning the temporary
    * directory.
    */
  def releaseAllLocks(): Unit = {
    localLocks.foreach { case (_, lock) =>
      lock.fileLock.foreach(_.release())
      lock.fileLock = None
    }
    localLocks.clear()
  }
}
