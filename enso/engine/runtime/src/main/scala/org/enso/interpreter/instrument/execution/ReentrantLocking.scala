package org.enso.interpreter.instrument.execution

import java.io.File
import java.util.UUID
import java.util.concurrent.locks.{Lock, ReentrantLock, ReentrantReadWriteLock}

/** Provides locking capabilities for the runtime server. Ir uses reentrant
  * locks.
  */
class ReentrantLocking extends Locking {

  private val compilationLock = new ReentrantReadWriteLock(true)

  private val pendingEditsLock = new ReentrantLock()

  private val contextMapLock = new ReentrantLock()

  private var contextLocks = Map.empty[UUID, ReentrantLock]

  private val fileMapLock = new ReentrantLock()

  private var fileLocks = Map.empty[File, ReentrantLock]

  protected def getContextLock(contextId: UUID): Lock = {
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

  protected def getFileLock(file: File): Lock = {
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
  override def acquireWriteCompilationLock(): Unit =
    compilationLock.writeLock().lockInterruptibly()

  /** @inheritdoc */
  override def releaseWriteCompilationLock(): Unit =
    compilationLock.writeLock().unlock()

  /** @inheritdoc */
  override def acquireReadCompilationLock(): Unit =
    compilationLock.readLock().lockInterruptibly()

  /** @inheritdoc */
  override def releaseReadCompilationLock(): Unit =
    compilationLock.readLock().unlock()

  /** @inheritdoc */
  override def acquirePendingEditsLock(): Unit =
    pendingEditsLock.lock()

  /** @inheritdoc */
  override def releasePendingEditsLock(): Unit =
    pendingEditsLock.unlock()

  /** @inheritdoc */
  override def acquireContextLock(contextId: UUID): Unit =
    getContextLock(contextId).lockInterruptibly()

  /** @inheritdoc */
  override def releaseContextLock(contextId: UUID): Unit =
    getContextLock(contextId).unlock()

  /** @inheritdoc */
  override def acquireFileLock(file: File): Unit =
    getFileLock(file).lockInterruptibly()

  /** @inheritdoc */
  override def releaseFileLock(file: File): Unit = getFileLock(file).unlock()

}
