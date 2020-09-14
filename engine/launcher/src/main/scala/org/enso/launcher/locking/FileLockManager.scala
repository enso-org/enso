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
  override def acquireLock(resourceName: String, shared: Boolean): Lock = {
    val channel = FileChannel.open(
      lockPath(resourceName),
      StandardOpenOption.CREATE,
      StandardOpenOption.WRITE
    )
    try {
      lockChannel(channel, shared)
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
    shared: Boolean
  ): Option[Lock] = {
    val channel = FileChannel.open(
      lockPath(resourceName),
      StandardOpenOption.CREATE,
      StandardOpenOption.WRITE
    )
    try {
      tryLockChannel(channel, shared)
    } catch {
      case NonFatal(e) =>
        channel.close()
        throw e
    }
  }

  private def lockPath(resourceName: String): Path =
    distributionManager.paths.locks.resolve(resourceName + ".lock")

  private def lockChannel(channel: FileChannel, shared: Boolean): Lock =
    WrapLock(channel.lock(0L, Long.MaxValue, shared))

  private def tryLockChannel(
    channel: FileChannel,
    shared: Boolean
  ): Option[Lock] =
    Option(channel.tryLock(0L, Long.MaxValue, shared)).map(WrapLock)

  private case class WrapLock(fileLock: FileLock) extends Lock {
    override def close(): Unit = fileLock.release()
  }
}
