package org.enso.runtimeversionmanager.locking

import java.nio.channels.{FileChannel, FileLock}
import java.nio.file.{Files, Path, StandardOpenOption}

import com.typesafe.scalalogging.Logger

import scala.util.control.NonFatal

/** [[LockManager]] using file-based locks - allowing to synchornize locks
  * between different processes.
  *
  * Under the hood, it uses the [[FileLock]] mechanism. This mechanism specifies
  * that on some platforms shared locks may not be supported and all shared
  * locks are treated as exclusive locks there. That would be harmful to the
  * user experience by allowing only a single Enso process to be running at the
  * same time as we use shared locks extensively. However, all the platforms we
  * currently support (Linux, macOS, Windows), do support shared locks, so this
  * should not be an issue for us. However, if a shared lock request returns an
  * exclusive lock, a warning is issued, just in case.
  *
  * The [[FileLock]] is not supposed to be used for synchronization of threads
  * within a single process. In fact using this manager to synchronize locks to
  * the same resources at the same time from different threads may result in
  * errors. Use-cases that may require access from multiple threads of a single
  * process should use [[ThreadSafeFileLockManager]] instead.
  *
  * @param locksRoot the directory in which lockfiles should be kept
  */
class FileLockManager(locksRoot: Path) extends LockManager {

  /** @inheritdoc */
  override def acquireLock(resourceName: String, lockType: LockType): Lock = {
    val channel = openChannel(resourceName)
    try {
      WrapLock(
        channel.lock(0L, Long.MaxValue, isShared(lockType)),
        channel,
        lockType
      )
    } catch {
      case NonFatal(e) =>
        channel.close()
        throw e
    }
  }

  /** @inheritdoc */
  override def tryAcquireLock(
    resourceName: String,
    lockType: LockType
  ): Option[Lock] = {
    val channel = openChannel(resourceName)
    try {
      val lock = channel.tryLock(0L, Long.MaxValue, isShared(lockType))
      if (lock == null) {
        channel.close()
        None
      } else {
        Some(WrapLock(lock, channel, lockType))
      }
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
    FileLockManager.lockPath(locksRoot, resourceName)

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

  /** Wraps a [[FileLock]] and the [[FileChannel]] that owns it, so that when
    * the lock is released, the owning channel is closed.
    */
  private case class WrapLock(
    fileLock: FileLock,
    channel: FileChannel,
    lockType: LockType
  ) extends Lock {

    if (isShared(lockType) && !fileLock.isShared) {
      Logger[FileLockManager].warn(
        "A request for a shared lock returned an exclusive lock. " +
        "The platform that you are running on may not support shared locks, " +
        "this may result in only a single Enso instance being able to run at " +
        "any time."
      )
    }

    override def release(): Unit = {
      fileLock.release()
      channel.close()
    }
  }
}

object FileLockManager {

  /** Resolves the path to the file that is used to synchronize access to a
    * resource between processes.
    *
    * @param locksRoot path to the root directory where locks are kept
    * @param resourceName name of the resource
    * @return path to the file which is locked to synchronize that resource
    */
  def lockPath(locksRoot: Path, resourceName: String): Path =
    locksRoot.resolve(resourceName + ".lock")
}
