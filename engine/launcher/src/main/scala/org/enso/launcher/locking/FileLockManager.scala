package org.enso.launcher.locking

import java.nio.channels.{FileChannel, FileLock}
import java.nio.file.{Files, Path, StandardOpenOption}

import scala.util.control.NonFatal

/**
  * [[LockManager]] using file-based locks.
  *
  * This is the [[LockManager]] that should be used in production as it is able
  * to synchronize locks between different processes.
  */
abstract class FileLockManager extends LockManager {

  /**
    * Specifies the directory in which lockfiles should be created.
    */
  def locksRoot: Path

  /**
    * @inheritdoc
    */
  override def acquireLock(resourceName: String, lockType: LockType): Lock = {
    val channel = openChannel(resourceName)
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
    val channel = openChannel(resourceName)
    try {
      tryLockChannel(channel, lockType)
    } catch {
      case NonFatal(e) =>
        channel.close()
        throw e
    }
  }

  private def isShared(lockType: LockType): Boolean =
    lockType match {
      case LockType.Exclusive => false
      case LockType.Shared    => true
    }

  private def lockPath(resourceName: String): Path =
    locksRoot.resolve(resourceName + ".lock")

  private def openChannel(resourceName: String): FileChannel = {
    val path   = lockPath(resourceName)
    val parent = path.getParent
    if (!Files.exists(parent)) {
      try Files.createDirectories(parent)
      catch { case NonFatal(_) => }
    }

    FileChannel.open(
      path,
      StandardOpenOption.CREATE,
      StandardOpenOption.READ,
      StandardOpenOption.WRITE
    )
  }

  private def lockChannel(channel: FileChannel, lockType: LockType): Lock = {
    WrapLock(channel.lock(0L, Long.MaxValue, isShared(lockType)), channel)
  }

  private def tryLockChannel(
    channel: FileChannel,
    lockType: LockType
  ): Option[Lock] =
    Option(channel.tryLock(0L, Long.MaxValue, isShared(lockType)))
      .map(WrapLock(_, channel))

  private case class WrapLock(fileLock: FileLock, channel: FileChannel)
      extends Lock {
    override def release(): Unit = {
      fileLock.release()
      channel.close()
    }
  }
}
