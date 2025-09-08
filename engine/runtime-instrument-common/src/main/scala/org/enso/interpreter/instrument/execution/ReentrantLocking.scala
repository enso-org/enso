package org.enso.interpreter.instrument.execution

import java.io.File
import java.util.UUID
import java.util.concurrent.Callable
import java.util.concurrent.locks.{Lock, ReentrantLock, ReentrantReadWriteLock}

import org.slf4j.Logger
import org.slf4j.LoggerFactory

/** Provides locking capabilities for the runtime server. Ir uses reentrant
  * locks.
  */
class ReentrantLocking extends Locking {
  private lazy val logger: Logger =
    LoggerFactory.getLogger(classOf[ReentrantLocking])

  /** Lowest level lock obtainable at any time. */
  private val pendingEditsLock = new ReentrantLock()

  /** Obtain anytime, except when holding pendingsEditLock. Guarded by fileMapLock. */
  private var fileLocks = Map.empty[File, ReentrantLock]

  /** Lower than contextLocks. Higher than anything else. */
  private val compilationLock = new ReentrantReadWriteLock(true)

  /** The highest lock. Always obtain first. Guarded by contextMapLock. */
  private var contextLocks = Map.empty[UUID, ContextLock]

  /** Guards contextLocks */
  private val contextMapLock = new ReentrantLock()

  /** Guards fileLocks */
  private val fileMapLock = new ReentrantLock()

  private def getContextLock(contextId: UUID): ContextLock = {
    contextMapLock.lock()
    try {
      if (contextLocks.contains(contextId)) {
        contextLocks(contextId)
      } else {
        val lock =
          new ContextLockImpl(new ReentrantReadWriteLock(true), contextId)
        contextLocks += (contextId -> lock)
        lock
      }
    } finally {
      contextMapLock.unlock()
    }
  }

  private def getFileLock(file: File): Lock = {
    fileMapLock.lock()
    try {
      if (fileLocks.contains(file)) {
        fileLocks(file)
      } else {
        val lock = new ReentrantLock(true)
        fileLocks += (file -> lock)
        lock
      }
    } finally {
      fileMapLock.unlock()
    }
  }

  private def acquireWriteCompilationLock(where: Class[_]): Long = {
    assertNotLocked(
      compilationLock,
      true,
      s"Cannot upgrade compilation read lock to write lock [${where.getSimpleName}]"
    )
    assertNotLocked(
      pendingEditsLock,
      s"Cannot acquire compilation write lock when having pending edits lock [${where.getSimpleName}]"
    )
    assertNoFileLock(
      s"Cannot acquire write compilation lock [${where.getSimpleName}]"
    )
    logLockAcquisition(compilationLock.writeLock(), "write compilation", where)
  }

  private def releaseWriteCompilationLock(): Unit =
    compilationLock.writeLock().unlock()

  override def assertWriteCompilationLock(): Unit =
    assert(
      compilationLock.writeLock().isHeldByCurrentThread,
      "should already held write compilation lock during the operation"
    )

  /** @inheritdoc */
  def withWriteCompilationLock[T](where: Class[_], callable: Callable[T]): T =
    withWriteCompilationLock(where, null, callable)

  /** @inheritdoc */
  def withWriteCompilationLock[T](
    where: Class[_],
    context: String,
    callable: Callable[T]
  ): T = {
    var lockTimestamp: Long = 0
    try {
      lockTimestamp = acquireWriteCompilationLock(where)
      callable.call()
    } catch {
      case _: InterruptedException =>
        logger.debug(
          "Failed [{}] to acquire lock: interrupted",
          where.getSimpleName
        )
        null.asInstanceOf[T]
    } finally {
      if (lockTimestamp != 0) {
        releaseWriteCompilationLock()
        logger.trace(
          "Kept write compilation lock [{}, context {}] for {}ms",
          where.getSimpleName,
          if (context == null) "<missing>" else context,
          System.currentTimeMillis - lockTimestamp
        )
      }
    }
  }

  private def acquireReadCompilationLock(where: Class[_]): Long = {
    // CloseFileCmd does:
    //   ctx.locking.acquireReadCompilationLock()
    //   ctx.locking.acquireFileLock(request.path)
    assertNoFileLock(
      s"Cannot acquire read compilation lock [${where.getSimpleName}]"
    )
    // CloseFileCmd also adds:
    //   ctx.locking.acquirePendingEditsLock()
    assertNotLocked(
      pendingEditsLock,
      s"Cannot acquire compilation read lock when having pending edits lock [${where.getSimpleName}]"
    )
    logLockAcquisition(compilationLock.readLock(), "read compilation", where)
  }

  private def releaseReadCompilationLock(): Unit =
    compilationLock.readLock().unlock()

  def withReadCompilationLock[T](where: Class[_], callable: Callable[T]): T = {
    var lockTimestamp: Long = 0
    try {
      lockTimestamp = acquireReadCompilationLock(where)
      callable.call()
    } catch {
      case _: InterruptedException =>
        logger.debug(
          "Failed [{}] to acquire lock: interrupted",
          where.getSimpleName
        )
        null.asInstanceOf[T]
    } finally {
      if (lockTimestamp != 0) {
        releaseReadCompilationLock()
        logger.trace(
          "Kept read compilation lock [{}] for {}ms",
          where.getSimpleName,
          System.currentTimeMillis - lockTimestamp
        )
      }
    }
  }

  private def acquirePendingEditsLock(where: Class[_]): Long = {
    logLockAcquisition(pendingEditsLock, "pending edit", where)
  }

  private def releasePendingEditsLock(): Unit =
    pendingEditsLock.unlock()

  /** @inheritdoc */
  override def withPendingEditsLock[T](
    where: Class[_],
    callable: Callable[T]
  ): T = {
    var lockTimestamp: Long = 0
    try {
      lockTimestamp = acquirePendingEditsLock(where)
      callable.call()
    } catch {
      case _: InterruptedException =>
        logger.debug(
          "Failed [{}] to acquire lock: interrupted",
          where.getSimpleName
        )
        null.asInstanceOf[T]
    } finally {
      if (lockTimestamp != 0) {
        releasePendingEditsLock()
        logger.trace(
          s"Kept pending edits lock [{}] for {}ms",
          where.getSimpleName,
          System.currentTimeMillis - lockTimestamp
        )
      }
    }
  }

  /** @inheritdoc */
  override def withReadContextLock[T](
    lock: ContextLock,
    where: Class[_],
    callable: Callable[T]
  ): T = {
    val contextLock                = lock.asInstanceOf[ContextLockImpl]
    var contextLockTimestamp: Long = 0
    val readLock                   = contextLock.lock.readLock()
    try {
      contextLockTimestamp = logLockAcquisition(
        readLock,
        "read context lock",
        where
      )
      callable.call()
    } catch {
      case _: InterruptedException =>
        logger.debug(
          "Failed [{}] to acquire lock: interrupted",
          where.getSimpleName
        )
        null.asInstanceOf[T]
    } finally {
      if (contextLockTimestamp != 0) {
        readLock.unlock()
        logger.trace(
          "Kept read context lock [{}] for {}ms",
          where.getSimpleName,
          System.currentTimeMillis - contextLockTimestamp
        )
      }
    }
  }

  /** @inheritdoc */
  override def withWriteContextLock[T](
    lock: ContextLock,
    where: Class[_],
    callable: Callable[T]
  ): T =
    withWriteContextLock(lock, where, null, callable)

  /** @inheritdoc */
  override def withWriteContextLock[T](
    lock: ContextLock,
    where: Class[_],
    context: String,
    callable: Callable[T]
  ): T = {
    val contextLock                = lock.asInstanceOf[ContextLockImpl]
    var contextLockTimestamp: Long = 0
    val writeLock                  = contextLock.lock.writeLock()
    val ctx                        = if (context == null) "<missing>" else context
    try {
      contextLockTimestamp = logLockAcquisition(
        writeLock,
        "write context lock (context=" + ctx + ")",
        where
      )
      callable.call()
    } catch {
      case _: InterruptedException =>
        logger.debug(
          "Failed [{}] to acquire lock: interrupted",
          where.getSimpleName
        )
        null.asInstanceOf[T]
    } finally {
      if (contextLockTimestamp != 0) {
        writeLock.unlock()
        logger.trace(
          "Kept write context lock [{}] for {}ms",
          where.getSimpleName,
          System.currentTimeMillis - contextLockTimestamp
        )
      }
    }
  }

  /** @inheritdoc */
  override def removeContextLock(lock: ContextLock): Unit = {
    val contextLock = lock.asInstanceOf[ContextLockImpl]
    contextMapLock.lock()
    try {
      if (contextLocks.contains(contextLock.uuid)) {
        assertNotLocked(
          contextLock.lock,
          true,
          s"Cannot remove context ${contextLock.uuid} lock when having a lock on it"
        )
        contextLocks -= contextLock.uuid
      }
    } finally {
      contextMapLock.unlock()
    }
  }

  private def acquireFileLock(file: File, where: Class[_]): Long = {
    // cannot have pendings lock as of EnsureCompiledJob.applyEdits
    assertNotLocked(
      pendingEditsLock,
      s"Cannot acquire [${where.getSimpleName}] file $file lock when having pending edits lock"
    )
    assertNoContextLock(s"Cannot acquire [${where.getSimpleName}] file $file")
    logLockAcquisition(getFileLock(file), "file", where)
  }

  private def releaseFileLock(file: File): Unit = getFileLock(file).unlock()

  /** @inheritdoc */
  override def withFileLock[T](
    file: File,
    where: Class[_],
    callable: Callable[T]
  ): T = {
    var lockTimestamp: Long = 0
    try {
      lockTimestamp = acquireFileLock(file, where)
      callable.call()
    } catch {
      case _: InterruptedException =>
        logger.debug(
          "Failed [{}] to acquire lock: interrupted",
          where.getSimpleName
        )
        null.asInstanceOf[T]
    } finally {
      if (lockTimestamp != 0) {
        releaseFileLock(file)
        logger.trace(
          s"Kept file lock [{}] for {}ms",
          where.getSimpleName,
          System.currentTimeMillis - lockTimestamp
        )
      }
    }
  }

  private def logLockAcquisition(
    lock: Lock,
    msg: String,
    where: Class[_]
  ): Long = {
    val now = System.currentTimeMillis()
    lock.lockInterruptibly()
    val now2 = System.currentTimeMillis()
    logger.trace(
      "Waited [{}] {}ms for the {}",
      where.getSimpleName,
      now2 - now,
      msg
    )
    now2
  }

  private def assertNotLocked(
    lock: ReentrantReadWriteLock,
    read: Boolean,
    msg: String
  ) = {
    val locked =
      if (read) lock.getReadHoldCount() > 0
      else lock.isWriteLockedByCurrentThread()
    if (locked) {
      throw new IllegalStateException(msg)
    }
  }
  private def assertNotLocked(lock: ReentrantLock, msg: String) = {
    if (lock.isHeldByCurrentThread()) {
      throw new IllegalStateException(msg)
    }
  }

  private def assertNoFileLock(msg: String) = {
    fileMapLock.lock()
    try {
      for (ctx <- fileLocks) {
        assertNotLocked(ctx._2, msg + s" when having file ${ctx._1} lock")
      }
    } finally {
      fileMapLock.unlock()
    }
  }

  private def assertNoContextLock(msg: String) = {
    contextMapLock.lock()
    try {
      for (ctx <- contextLocks) {
        val contextLock = ctx._2.asInstanceOf[ContextLockImpl]
        assertNotLocked(
          contextLock.lock,
          true,
          msg + s" lock when having context ${ctx._1} lock"
        )
      }
    } finally {
      contextMapLock.unlock()
    }
  }

  override def getOrCreateContextLock(contextId: UUID): ContextLock = {
    assertNotLocked(
      compilationLock,
      true,
      s"Cannot acquire context ${contextId} lock when having compilation read lock"
    )
    assertNotLocked(
      compilationLock,
      false,
      s"Cannot acquire context ${contextId} lock when having compilation write lock"
    )
    assertNoFileLock(s"Cannot acquire context ${contextId}")
    assertNotLocked(
      pendingEditsLock,
      s"Cannot acquire context ${contextId} lock when having pending edits lock"
    )
    getContextLock(contextId)
  }

  private case class ContextLockImpl(lock: ReentrantReadWriteLock, uuid: UUID)
      extends ContextLock
}
