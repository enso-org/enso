package org.enso.runtimeversionmanager.test

import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.{
  ReadWriteLock,
  ReentrantReadWriteLock,
  Lock => JLock
}

import org.enso.runtimeversionmanager.locking.{Lock, LockManager, LockType}

/** A [[LockManager]] that creates process-local locks.
  *
  * The locks are not visible by other processes, so this manager is not useful
  * for synchronizing multiple processes. It can be used to test concurrency
  * implementation using threads within the same JVM.
  *
  * Normally [[acquireLock]] would wait forever until the lock can be acquired
  * or the thread is interrupted. To aid with testing, this implementation times
  * out after 30 seconds.
  */
class TestLocalLockManager extends LockManager {

  /** @inheritdoc
    */
  override def acquireLock(resourceName: String, lockType: LockType): Lock = {
    val lock   = getLock(resourceName, lockType)
    val locked = lock.tryLock(30, TimeUnit.SECONDS)
    if (!locked) {
      throw new RuntimeException(
        "Acquiring the lock took more than 30s, perhaps a deadlock?"
      )
    }
    WrapLock(lock)
  }

  /** @inheritdoc
    */
  override def tryAcquireLock(
    resourceName: String,
    lockType: LockType
  ): Option[Lock] = {
    val lock = getLock(resourceName, lockType)
    if (lock.tryLock()) Some(WrapLock(lock))
    else None
  }

  case class WrapLock(lock: JLock) extends Lock {
    override def release(): Unit = lock.unlock()
  }

  private val locks: collection.concurrent.Map[String, ReadWriteLock] =
    collection.concurrent.TrieMap()

  private def getLock(resourceName: String, lockType: LockType): JLock = {
    val rwLock =
      locks.getOrElseUpdate(resourceName, new ReentrantReadWriteLock(true))
    lockType match {
      case LockType.Exclusive => rwLock.writeLock()
      case LockType.Shared    => rwLock.readLock()
    }
  }
}
