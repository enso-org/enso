package org.enso.launcher.locking

import java.nio.channels.{FileChannel, FileLock}
import java.nio.file.{Path, StandardOpenOption}

import org.enso.launcher.installation.DistributionManager

import scala.util.control.NonFatal

class FileLockManager(distributionManager: DistributionManager)
    extends LockManager {
  // TODO [RW] may want to figure out when are the locks deleted, maybe add a
  //  cleanup method that tries to delete (and fails silently on locked files)

  /**
    * @inheritdoc
    */
  override def acquireLock(resourceName: String, lockType: LockType): Lock = {
    val channel = FileChannel.open(
      lockPath(resourceName),
      StandardOpenOption.CREATE,
      StandardOpenOption.WRITE
    )
    try {
      lockChannel(channel, lockType)
    } catch {
      case NonFatal(e) =>
        channel.close()
        throw e
    }
  }

  /**
    * @inheritdoc
    */
  override def tryAcquireLock(
    resourceName: String,
    lockType: LockType
  ): Option[Lock] = {
    val channel = FileChannel.open(
      lockPath(resourceName),
      StandardOpenOption.CREATE,
      StandardOpenOption.WRITE
    )
    try {
      tryLockChannel(channel, lockType)
    } catch {
      case NonFatal(e) =>
        channel.close()
        throw e
    }
  }

  private def lockPath(resourceName: String): Path =
    distributionManager.paths.locks.resolve(resourceName + ".lock")

  private def isShared(lockType: LockType): Boolean =
    lockType match {
      case LockType.Exclusive => false
      case LockType.Shared    => true
    }

  private def lockChannel(channel: FileChannel, lockType: LockType): Lock =
    WrapLock(channel.lock(0L, Long.MaxValue, isShared(lockType)))

  private def tryLockChannel(
    channel: FileChannel,
    lockType: LockType
  ): Option[Lock] =
    Option(channel.tryLock(0L, Long.MaxValue, isShared(lockType))).map(WrapLock)

  private case class WrapLock(fileLock: FileLock) extends Lock {
    override def release(): Unit = fileLock.release()
  }
}
