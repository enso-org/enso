package org.enso.interpreter.instrument.execution

import com.oracle.truffle.api.TruffleLogger

import java.io.File
import java.util.UUID
import java.util.concurrent.locks.{Lock, ReentrantLock, ReentrantReadWriteLock}
import java.util.logging.Level

/** Provides locking capabilities for the runtime server. Ir uses reentrant
  * locks.
  */
class ReentrantLocking(logger: TruffleLogger) extends Locking {

  /** Lowest level lock obtainable at any time. */
  private val pendingEditsLock = new ReentrantLock()

  /** Obtain anytime, except when holding pendingsEditLock. Guarded by fileMapLock. */
  private var fileLocks = Map.empty[File, ReentrantLock]

  /** Lower than contextLocks. Higher than anything else. */
  private val compilationLock = new ReentrantReadWriteLock(true)

  /** The highest lock. Always obtain first. Guarded by contextMapLock. */
  private var contextLocks = Map.empty[UUID, ReentrantLock]

  /** Guards contextLocks */
  private val contextMapLock = new ReentrantLock()

  /** Guards fileLocks */
  private val fileMapLock = new ReentrantLock()

  private def getContextLock(contextId: UUID): Lock = {
    contextMapLock.lock()
    try {
      if (contextLocks.contains(contextId)) {
        contextLocks(contextId)
      } else {
        val lock = new ReentrantLock(true)
        contextLocks += (contextId -> lock)
        lock
      }
    } finally {
      contextMapLock.unlock()
    }
  }

  /** @inheritdoc */
  override def removeContextLock(contextId: UUID): Unit = {
    contextMapLock.lock()
    try {
      contextLocks -= contextId
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

  /** @inheritdoc */
  override def removeFileLock(file: File): Unit = {
    fileMapLock.lock()
    try {
      fileLocks -= file
    } finally {
      fileMapLock.unlock()
    }
  }

  /** @inheritdoc */
  override def acquireWriteCompilationLock(): Long = {
    assertNotLocked(
      compilationLock,
      true,
      "Cannot upgrade compilation read lock to write lock"
    )
    assertNotLocked(
      pendingEditsLock,
      s"Cannot acquire compilation write lock when having pending edits lock"
    )
    assertNoFileLock("Cannot acquire write compilation lock")
    logLockAcquisition(compilationLock.writeLock(), "write compilation")
  }

  /** @inheritdoc */
  override def releaseWriteCompilationLock(): Unit =
    compilationLock.writeLock().unlock()

  /** @inheritdoc */
  override def acquireReadCompilationLock(): Long = {
    // CloseFileCmd does:
    //   ctx.locking.acquireReadCompilationLock()
    //   ctx.locking.acquireFileLock(request.path)
    assertNoFileLock("Cannot acquire read compilation lock")
    // CloseFileCmd also adds:
    //   ctx.locking.acquirePendingEditsLock()
    assertNotLocked(
      pendingEditsLock,
      s"Cannot acquire compilation read lock when having pending edits lock"
    )
    logLockAcquisition(compilationLock.readLock(), "read compilation")
  }

  /** @inheritdoc */
  override def releaseReadCompilationLock(): Unit =
    compilationLock.readLock().unlock()

  /** @inheritdoc */
  override def acquirePendingEditsLock(): Long = {
    logLockAcquisition(pendingEditsLock, "pending edit")
  }

  /** @inheritdoc */
  override def releasePendingEditsLock(): Unit =
    pendingEditsLock.unlock()

  /** @inheritdoc */
  override def acquireContextLock(contextId: UUID): Long = {
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
    logLockAcquisition(getContextLock(contextId), s"$contextId context")
  }

  /** @inheritdoc */
  override def releaseContextLock(contextId: UUID): Unit =
    getContextLock(contextId).unlock()

  /** @inheritdoc */
  override def acquireFileLock(file: File): Long = {
    // cannot have pendings lock as of EnsureCompiledJob.applyEdits
    assertNotLocked(
      pendingEditsLock,
      s"Cannot acquire file ${file} lock when having pending edits lock"
    )
    assertNoContextLock(s"Cannot acquire file ${file}")
    logLockAcquisition(getFileLock(file), "file")
  }

  /** @inheritdoc */
  override def releaseFileLock(file: File): Unit = getFileLock(file).unlock()

  private def logLockAcquisition(lock: Lock, msg: String): Long = {
    val now = System.currentTimeMillis()
    lock.lockInterruptibly()
    val now2 = System.currentTimeMillis()
    logger.log(Level.FINEST, s"Waited ${now2 - now} for the $msg lock")
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
        assertNotLocked(
          ctx._2,
          msg + s" lock when having context ${ctx._1} lock"
        )
      }
    } finally {
      contextMapLock.unlock()
    }
  }
}
