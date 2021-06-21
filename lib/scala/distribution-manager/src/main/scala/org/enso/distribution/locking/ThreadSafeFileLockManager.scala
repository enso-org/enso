package org.enso.distribution.locking

import java.nio.file.Path

/** A class that allows to manage locks which synchronize access both between
  * different processes and multiple threads of the current process.
  *
  * It does not have as strong fairness guarantees as [[FileLockManager]], for
  * example, when other processes are holding a shared lock on a resource and
  * the current process tries to acquire an exclusive lock, other threads of the
  * current process will not be able to acquire even shared locks until this
  * other threads acquires and then releases its writer lock (they will wait for
  * it).
  *
  * This weak-fairness issue should not be problematic as it only happens when
  * the same process tries to acquire an exclusive and shared lock for the same
  * resource. This can happen in two kinds of situations:
  * - a short update (updating the config)
  * - a long running operation (install/uninstall a component).
  * In the first case, both reads and writes are short, so scheduling them is
  * not an issue because the wait time will be marginal anyway. In the second
  * case, opening a project while another thread is trying to uninstall the
  * engine (and actively waiting for other processes), will wait until that
  * completes and fail or try to install it again. But that is only the case if
  * a single process both requests the uninstall of an engine and opening a
  * project with the same engine, which from the user's perspective is extremely
  * unlikely (why uninstall an engine and at the same time try to open a project
  * using it).
  *
  * Functions that acquire locks in this class may throw (for example if the
  * underlying file lock fails with an IOException), but if a non-fatal
  * exception is thrown, the function is guaranteed to not have acquired a lock.
  *
  * @param locksRoot the directory in which lockfiles should be kept
  */
class ThreadSafeFileLockManager(locksRoot: Path) extends LockManager {
  val fileLockManager = new FileLockManager(locksRoot)
  val localLocks =
    collection.concurrent.TrieMap.empty[String, ThreadSafeLock]

  /** A thread-safe wrapper for a file lock - ensures that the process holds at
    * most one file lock for the file, but it can be shared by multiple threads
    * which are properly synchronized.
    *
    * The structure maintains the following invariants (they should always be
    * valid before entering and after exiting the monitor):
    * - either writers or readers can be nonzero, but never both at the same
    *   time
    * - there can be at most 1 writer
    * - the fileLock is Some iff writers + readers > 0
    * - busy == true indicates that one of the threads is waiting to acquire the
    *   proper file lock - no other operations should be taken during that time:
    *   they should either wait or fail immediately, depending on the operation
    *   type
    * - any `try*` operations should fail if busy is set to true
    * - busy == true implies readers + writers == 0
    *   - because the writer lock is only tried to be acquired if
    *     readers + writers == 0
    *   - and the reader filelock is only tried to be acquired if readers == 0
    *    (and writers == 0), as if readers > 0, the filelock is already held
    *   - conversely, readers + writers > 0 implies busy == false
    *
    * The purpose of the busy bit is to avoid long blocking the monitor while
    * waiting to acquire the actual file lock, to guarantee [[tryAcquireLock]]
    * returning without blocking.
    *
    * The busy bit is guaranteed to be reset eventually (it will either reset
    * once the acquireLock operation completes or when it fails and throws an
    * exception).
    *
    * If a release operation fails, we cannot guarantee anything about the
    * status of the resource, so any pending and future operations fail with an
    * exception immediately. If failed == true, the invariants may no longer
    * hold - every operation (besides release) should run `checkFailed` first.
    */
  class ThreadSafeLock(name: String) { self =>
    @volatile var busy: Boolean   = false
    @volatile var failed: Boolean = false
    var writers: Int              = 0
    var readers: Int              = 0
    var fileLock: Option[Lock]    = None

    /** Checks if the lock has been set to failed state and throws an exception
      * if that is the case.
      */
    def checkFailed(): Unit = {
      if (failed)
        throw new IllegalStateException(
          "A release operation has failed and the resource is in an unknown " +
          "state."
        )
    }

    /** Decrements the reader count and releases the lock if it was the last
      * reader.
      *
      * Should be called at most once by each reader.
      */
    def releaseReader(): Unit = this.synchronized {
      assert(writers == 0)
      assert(readers > 0)
      assert(fileLock.isDefined)
      assert(!busy)
      readers -= 1

      if (readers == 0) {
        try {
          fileLock.get.release()
          fileLock = None
        } catch {
          case e: Throwable =>
            failed = true
            this.notifyAll()
            throw e
        }
      }

      this.notifyAll()
    }

    /** Decrements the writer count and releases the exclusive lock.
      *
      * Should be called at most once by each writer.
      */
    def releaseWriter(): Unit = this.synchronized {
      assert(readers == 0)
      assert(writers == 1)
      assert(fileLock.isDefined)
      assert(!busy)
      writers -= 1
      try {
        fileLock.get.release()
        fileLock = None
      } catch {
        case e: Throwable =>
          failed = true
          this.notifyAll()
          throw e
      }

      this.notifyAll()
    }

    /** Tries to acquire the exclusive lock, succeeding immediately if no one
      * else is using the resource.
      */
    def tryAcquireWriter(): Option[Lock] = this.synchronized {
      checkFailed()
      if (!busy && readers == 0 && writers == 0) {
        assert(fileLock.isEmpty)
        fileLock = fileLockManager.tryAcquireLock(name, LockType.Exclusive)
        if (fileLock.isDefined) {
          writers += 1
          Some(new WriterReleaser)
        } else None
      } else None
    }

    /** Tries to acquire the shared lock, succeeding immediately if no one
      * else is using the resource.
      */
    def tryAcquireReader(): Option[Lock] = this.synchronized {
      checkFailed()
      if (!busy && writers == 0) {
        val noOtherReaders = readers == 0
        if (noOtherReaders) {
          assert(fileLock.isEmpty)
          fileLock = fileLockManager.tryAcquireLock(name, LockType.Shared)
        }

        if (fileLock.isDefined) {
          readers += 1
          Some(new ReaderReleaser)
        } else None

      } else None
    }

    /** Acquires an exclusive lock.
      *
      * First waits until no other threads are busy or holding any locks. It
      * sets the busy bit and releases the monitor (so that tryAcquire will be
      * able to enter it and fail immediately seeing the busy bit, instead of
      * waiting). After acquiring the file lock (which may take some time as
      * other processes may also be holding it), the busy bit is cleared and
      * writer count updated.
      */
    def acquireWriter(): Lock = {
      this.synchronized {
        while ({ checkFailed(); busy || (readers + writers) > 0 }) {
          this.wait()
        }
        assert(!busy)
        assert(readers + writers == 0)
        assert(fileLock.isEmpty)
        busy = true
      }

      val lock =
        try { fileLockManager.acquireLock(name, LockType.Exclusive) }
        catch {
          case e: Throwable =>
            busy = false
            throw e
        }

      this.synchronized {
        assert(busy)
        assert(readers + writers == 0)
        assert(fileLock.isEmpty)
        busy = false
        writers += 1
        fileLock = Some(lock)
        new WriterReleaser
      }
    }

    /** Acquires a shared lock.
      *
      * First waits until no other threads are busy or holding writer locks. If
      * any other threads already hold a reader lock, it means that the file
      * lock is already held, so we can just increase the counter and return
      * immediately. Otherwise, we set the busy bit and release the monitor,
      * similarly as in [[acquireWriter]]. Once the actual file lock is
      * acquired, we update the reader count and clear the busy bit.
      */
    def acquireReader(): Lock = {
      this.synchronized {
        while ({ checkFailed(); busy || writers > 0 }) {
          this.wait()
        }
        assert(!busy)
        assert(writers == 0)

        if (readers > 0) {
          readers += 1
          return new ReaderReleaser
        } else {
          assert(fileLock.isEmpty)
          busy = true
        }
      }

      val lock =
        try { fileLockManager.acquireLock(name, LockType.Shared) }
        catch {
          case e: Throwable =>
            busy = false
            throw e
        }

      this.synchronized {
        assert(busy)
        assert(readers + writers == 0)
        assert(fileLock.isEmpty)
        busy = false
        readers += 1
        fileLock = Some(lock)
        new ReaderReleaser
      }
    }

    private class WriterReleaser extends ReleaseOnceLockWrapper {
      override protected def doRelease(): Unit = self.releaseWriter()
    }

    private class ReaderReleaser extends ReleaseOnceLockWrapper {
      override protected def doRelease(): Unit = self.releaseReader()
    }
  }

  /** A simple wrapper that ensures that the lock is released at most once.
    *
    * This is to ensure the invariants of [[ThreadSafeLock]] without relying on
    * the user calling `release` not more than once. Instead, this wrapper can
    * be called multiple times, but the actual lock is only released the first
    * time.
    */
  abstract private class ReleaseOnceLockWrapper extends Lock {

    /** Actually release the lock.
      *
      * This function is called at most once.
      */
    protected def doRelease(): Unit

    private var released: Boolean = false

    /** @inheritdoc */
    override def release(): Unit = this.synchronized {
      if (!released) {
        released = true
        doRelease()
      }
    }
  }

  /** Returns a unique lock used for synchronizing the resource with the given
    * name.
    *
    * Locks are never removed, even if no threads are using them anymore, but
    * the amount of resources that we use is very small so this is not a
    * problem.
    */
  private def getLock(name: String): ThreadSafeLock =
    localLocks.getOrElseUpdate(name, new ThreadSafeLock(name))

  /** @inheritdoc */
  override def acquireLock(resourceName: String, lockType: LockType): Lock = {
    val lock = getLock(resourceName)
    lockType match {
      case LockType.Exclusive => lock.acquireWriter()
      case LockType.Shared    => lock.acquireReader()
    }
  }

  /** @inheritdoc */
  override def tryAcquireLock(
    resourceName: String,
    lockType: LockType
  ): Option[Lock] = {
    val lock = getLock(resourceName)
    lockType match {
      case LockType.Exclusive => lock.tryAcquireWriter()
      case LockType.Shared    => lock.tryAcquireReader()
    }
  }
}
