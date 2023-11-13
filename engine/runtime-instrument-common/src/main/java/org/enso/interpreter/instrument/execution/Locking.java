package org.enso.interpreter.instrument.execution;

import java.io.File;
import java.util.UUID;

/** Provides locking capabilities for the runtime server. */
public interface Locking {

  /**
   * Acquires a compilation write lock and returns a timestamp when it succeeded.
   *
   * @return timestamp of when the lock was acquired
   */
  long acquireWriteCompilationLock() throws InterruptedException;

  /** Releases a compilation write lock. */
  void releaseWriteCompilationLock();

  /**
   * Acquires a compilation read lock and returns a timestamp when it succeeded.
   *
   * @return timestamp of when the lock was acquired
   */
  long acquireReadCompilationLock() throws InterruptedException;

  /** Releases a compilation read lock. */
  void releaseReadCompilationLock();

  /**
   * Acquires a pending edits lock and returns a timestamp when it succeeded.
   *
   * @return timestamp of when the lock was acquired
   */
  long acquirePendingEditsLock() throws InterruptedException;

  /** Releases a pending edits lock. */
  void releasePendingEditsLock();

  /**
   * Acquires a context lock and returns a timestamp when it succeeded.
   *
   * @param contextId a context to lock
   * @return timestamp of when the lock was acquired
   */
  long acquireContextLock(UUID contextId) throws InterruptedException;

  /**
   * Releases a context lock.
   *
   * @param contextId a context to unlock
   */
  void releaseContextLock(UUID contextId);

  /**
   * Releases and removes a context lock.
   *
   * @param contextId a context to unlock
   */
  void removeContextLock(UUID contextId);

  /**
   * Acquires a file lock and returns a timestamp when it succeeded.
   *
   * @param file a file to lock
   * @return timestamp of when the lock was acquired
   */
  long acquireFileLock(File file) throws InterruptedException;

  /**
   * Releases a file lock.
   *
   * @param file a file to unlock
   */
  void releaseFileLock(File file);
}
