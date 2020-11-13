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

  /** Acquires a compilation write lock.
    */
  def acquireWriteCompilationLock(): Unit

  /** Releases a compilation write lock.
    */
  def releaseWriteCompilationLock(): Unit

  /** Acquires a compilation read lock.
    */
  def acquireReadCompilationLock(): Unit

  /** Releases a compilation read lock.
    */
  def releaseReadCompilationLock(): Unit

  /** Acquires a context lock.
    *
    * @param contextId a context to lock
    */
  def acquireContextLock(contextId: UUID): Unit

  /** Releases a context lock.
    *
    * @param contextId a context to unlock
    */
  def releaseContextLock(contextId: UUID): Unit

  /** Acquires a file lock.
    *
    * @param file a file to lock
    */
  def acquireFileLock(file: File): Unit

  /** Releases a file lock.
    *
    * @param file a file to unlock
    */
  def releaseFileLock(file: File): Unit

}
