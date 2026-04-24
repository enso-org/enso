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
   * Executes `callable` while holding a compilation write lock
   *
   * @param where the class requesting the lock
   * @param callable code to be executed while holding the lock
   * @param context human-readable explanation for triggering evaluation
   * @return the result of calling `callable` or null, if no result is expected
   */
  <T> T withWriteCompilationLock(Class<?> where, String context, Callable<T> callable);

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
   * Executes `callable` while holding a read context lock
   *
   * @param contextLock lock used to ensure exclusive access
   * @param where the class requesting the lock
   * @param callable code to be executed while holding the lock
   * @return the result of calling `callable` or null, if no result is expected
   */
  <T> T withReadContextLock(ContextLock contextLock, Class<?> where, Callable<T> callable);

  /**
   * Executes `callable` while holding a write context lock
   *
   * @param contextLock lock used to ensure exclusive access
   * @param where the class requesting the lock
   * @param callable code to be executed while holding the lock
   * @return the result of calling `callable` or null, if no result is expected
   */
  <T> T withWriteContextLock(ContextLock contextLock, Class<?> where, Callable<T> callable);

  /**
   * Executes `callable` while holding a write context lock
   *
   * @param contextLock lock used to ensure exclusive access
   * @param where the class requesting the lock
   * @param context human-readable explanation for lock
   * @param callable code to be executed while holding the lock
   * @return the result of calling `callable` or null, if no result is expected
   */
  <T> T withWriteContextLock(
      ContextLock contextLock, Class<?> where, String context, Callable<T> callable);

  /**
   * Removes a context lock.
   *
   * @param contextLock a context lock to remove
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

  /**
   * If one can enter write compilation lock without blocking, then invokes {@code action.run()}
   * while holding the lock and releasing it then. In such case this method returns {@code true}.
   * Otherwise it performs no action and returns {@code false}.
   *
   * @param where the class requesting the lock
   * @param action code to be executed while holding the lock
   * @return {@code true} if {@code action} was executed or {@code false} otherwise
   */
  boolean tryWithWriteCompilationLock(Class<?> where, Runnable action);

  /**
   * If one can enter read context lock without blocking, then invokes {@code action.run()} while
   * holding the lock and releasing it then. In such case this method returns {@code true}.
   * Otherwise it performs no action and returns {@code false}.
   *
   * @param contextLock lock used to ensure exclusive access
   * @param where the class requesting the lock
   * @param action code to be executed while holding the lock
   * @return {@code true} if {@code action} was executed or {@code false} otherwise
   */
  boolean tryWithReadContextLock(ContextLock contextLock, Class<?> where, Runnable action);
}
