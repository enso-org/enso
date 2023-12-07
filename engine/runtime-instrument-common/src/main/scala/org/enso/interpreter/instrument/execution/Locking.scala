package org.enso.interpreter.instrument.execution

import java.io.File
import java.util.UUID

/** Provides locking capabilities for the runtime server.
  */
trait Locking {

  /** Removes a lock for an execution context.
    *
    * @param contextId the context id
    */
  def removeContextLock(contextId: UUID): Unit

  /** Removes a lock for a file that contains enso code.
    *
    * @param file the file to lock
    */
  def removeFileLock(file: File): Unit

  /** Acquires a compilation write lock and returns a timestamp when it succeeded.
    *
    * @return timestamp of when the lock was acquired
    */
  def acquireWriteCompilationLock(): Long

  /** Releases a compilation write lock.
    */
  def releaseWriteCompilationLock(): Unit

  /** Aseerts a compilation write lock is held by the current thread.
    */
  def assertWriteCompilationLock(): Unit

  /** Acquires a compilation read lock and returns a timestamp when it succeeded.
    *
    * @return timestamp of when the lock was acquired
    */
  def acquireReadCompilationLock(): Long

  /** Releases a compilation read lock.
    */
  def releaseReadCompilationLock(): Unit

  /** Acquires a pending edits lock and returns a timestamp when it succeeded.
    *
    * @return timestamp of when the lock was acquired
    */
  def acquirePendingEditsLock(): Long

  /** Releases a pending edits lock.
    */
  def releasePendingEditsLock(): Unit

  /** Acquires a context lock and returns a timestamp when it succeeded.
    *
    * @param contextId a context to lock
    * @return timestamp of when the lock was acquired
    */
  def acquireContextLock(contextId: UUID): Long

  /** Releases a context lock.
    *
    * @param contextId a context to unlock
    */
  def releaseContextLock(contextId: UUID): Unit

  /** Acquires a file lock and returns a timestamp when it succeeded.
    *
    * @param file a file to lock
    * @return timestamp of when the lock was acquired
    */
  def acquireFileLock(file: File): Long

  /** Releases a file lock.
    *
    * @param file a file to unlock
    */
  def releaseFileLock(file: File): Unit

}
