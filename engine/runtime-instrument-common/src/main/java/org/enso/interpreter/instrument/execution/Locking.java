package org.enso.interpreter.instrument.execution;

import java.io.File;
import java.util.UUID;
import java.util.concurrent.Callable;

/** Provides locking capabilities for the runtime server. */
public interface Locking {

  void assertWriteCompilationLock();

  /**
   * Executes `callable` while holding a compilation write lock
   *
   * @param where the class requesting the lock
   * @param callable code to be executed while holding the lock
   * @return the result of calling `callable` or null, if no result is expected
   */
  <T> T withWriteCompilationLock(Class<?> where, Callable<T> callable);

  /**
   * Executes `callable` while holding a compilation read lock
   *
   * @param where the class requesting the lock
   * @param callable code to be executed while holding the lock
   * @return the result of calling `callable` or null, if no result is expected
   */
  <T> T withReadCompilationLock(Class<?> where, Callable<T> callable);

  /**
   * Executes `callable` while holding a pending edits lock
   *
   * @param where the class requesting the lock
   * @param callable code to be executed while holding the lock
   * @return the result of calling `callable` or null, if no result is expected
   */
  <T> T withPendingEditsLock(Class<?> where, Callable<T> callable);

  /**
   * Executes `callable` while holding a context lock
   *
   * @param contextLock lock used to ensure exclusive access
   * @param where the class requesting the lock
   * @param callable code to be executed while holding the lock
   * @return the result of calling `callable` or null, if no result is expected
   */
  <T> T withContextLock(ContextLock contextLock, Class<?> where, Callable<T> callable);

  /**
   * Removes a context lock.
   *
   * @param a context lock to remove
   */
  void removeContextLock(ContextLock contextLock);

  /**
   * Executes `callable` while holding a file lock
   *
   * @param file for which the lock is being requested
   * @param where the class requesting the lock
   * @param callable code to be executed while holding the lock
   * @return the result of calling `callable` or null, if no result is expected
   */
  <T> T withFileLock(File file, Class<?> where, Callable<T> callable);

  /**
   * Gets an existing context lock, or creates a fresh one, for the given context ID.
   *
   * @param contextId context id for which a lock will be returned
   * @return lock wrapper
   */
  ContextLock getOrCreateContextLock(UUID contextId);
}
