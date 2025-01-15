package org.enso.interpreter.runtime;

import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import org.enso.pkg.QualifiedName;

/**
 * Manages threading aspects of serialization. The goal of {@code SerializationPool} is to
 * encapsulate working with threads:
 *
 * <ul>
 *   <li>serialization is done asychronously in a single background thread
 *   <li>deserialization is done synchronously and tries to wait for possible background work to
 *       finish
 * </ul>
 *
 * It is good to keep in mind, that serialization isn't the primary goal while Enso program is
 * running. When a program is running as much of the CPU time should be dedicated to compilation and
 * execution. Only when the Enso program execution is over, flushing the pending caches becomes a
 * priority. Future rewrites of this class may optimize towards such direction.
 */
final class SerializationPool {
  private final TruffleCompilerContext context;

  /**
   * A set of the modules that are currently being serialized.
   *
   * <p>This set is accessed concurrently. This is safe as it is backed by a [[ConcurrentHashMap]]
   * and is wrapped with the scala [[mutable.Set]] interface.
   */
  private final Map<QualifiedName, Boolean> isSerializing = new ConcurrentHashMap<>();

  /**
   * A map of the modules awaiting serialization to their associated tasks
   *
   * <p>This map is accessed concurrently.
   */
  private final Map<QualifiedName, Future<?>> isWaitingForSerialization = new ConcurrentHashMap<>();

  /** The thread pool that handles serialization. */
  private final ExecutorService pool;

  /** all associated threads */
  private final Set<Thread> threads = Collections.synchronizedSet(new HashSet<>());

  SerializationPool(TruffleCompilerContext context) {
    this.context = context;
    this.pool =
        Executors.newSingleThreadExecutor(
            (r) -> {
              var t = context.createSystemThread(r);
              t.setName("SerializationPool background thread");
              threads.add(t);
              return t;
            });
  }

  /**
   * @return `true` if there are remaining serialization jobs, `false` otherwise
   */
  private boolean hasJobsRemaining() {
    synchronized (isWaitingForSerialization) {
      return !isWaitingForSerialization.isEmpty() || !isSerializing.isEmpty();
    }
  }

  /**
   * Performs shutdown actions for the serialization manager.
   *
   * @param waitForPendingJobCompletion whether or not shutdown should wait for pending
   *     serialization jobs
   */
  void shutdown(boolean waitForPendingJobCompletion) throws InterruptedException {
    if (!pool.isShutdown()) {
      if (waitForPendingJobCompletion && this.hasJobsRemaining()) {
        int waitingCount;
        int jobCount;
        synchronized (isWaitingForSerialization) {
          waitingCount = isWaitingForSerialization.size();
          jobCount = waitingCount + isSerializing.size();
        }
        context.logSerializationManager(
            Level.FINE, "Waiting for #{0} serialization jobs to complete.", jobCount);

        // Bound the waiting loop
        int maxCount = 60;
        int counter = 0;
        while (this.hasJobsRemaining() && counter < maxCount) {
          counter += 1;
          synchronized (isWaitingForSerialization) {
            isWaitingForSerialization.wait(1000);
          }
        }
      }

      pool.shutdown();

      // Bound the waiting loop
      int maxCount = 10;
      int counter = 0;
      while (!pool.isTerminated() && counter < maxCount) {
        pool.awaitTermination(500, TimeUnit.MILLISECONDS);
        counter += 1;
      }

      pool.shutdownNow();
      context.logSerializationManager(Level.FINE, "Serialization manager shutdownNow.");

      for (var t : threads.toArray(new Thread[0])) {
        context.logSerializationManager(Level.FINEST, "Serialization manager has been shut down.");
        t.join();
      }
      context.logSerializationManager(Level.FINE, "Serialization manager has been shut down.");
      threads.clear();
    }
  }

  boolean isWaitingForSerialization(QualifiedName key) {
    synchronized (isWaitingForSerialization) {
      return isWaitingForSerialization.containsKey(key);
    }
  }

  /**
   * Checks if the provided key is waiting for serialization.
   *
   * @param key the library to check
   * @return {@code true} if there is a pending serialization for given key, {@code false} otherwise
   */
  boolean abort(QualifiedName key) {
    synchronized (isWaitingForSerialization) {
      if (isWaitingForSerialization(key)) {
        var prev = isWaitingForSerialization.remove(key);
        isWaitingForSerialization.notifyAll();
        if (prev != null) {
          return prev.cancel(false);
        } else {
          return false;
        }
      } else {
        return false;
      }
    }
  }

  void startSerializing(QualifiedName name) {
    synchronized (isWaitingForSerialization) {
      isWaitingForSerialization.remove(name);
      isSerializing.put(name, true);
      isWaitingForSerialization.notifyAll();
    }
  }

  /**
   * Sets the {@code key} as finished with serialization.
   *
   * @param name the key to set as having finished serialization
   */
  void finishSerializing(QualifiedName name) {
    synchronized (isWaitingForSerialization) {
      isSerializing.remove(name);
      isWaitingForSerialization.notifyAll();
    }
  }

  <T> Future<T> submitTask(Callable<T> task, boolean useThreadPool, QualifiedName key) {
    if (useThreadPool) {
      synchronized (isWaitingForSerialization) {
        var future = pool.submit(task);
        isWaitingForSerialization.put(key, future);
        return future;
      }
    } else {
      try {
        return CompletableFuture.completedFuture(task.call());
      } catch (Throwable e) {
        context.logSerializationManager(
            Level.WARNING, "Serialization task failed for [" + key + "].", e);
        return CompletableFuture.failedFuture(e);
      }
    }
  }

  /**
   * Waits for a given key to finish serialization, if there is one pending.
   *
   * @param name the key
   * @throws InterruptedException if the wait is interrupted
   */
  void waitWhileSerializing(QualifiedName name) throws InterruptedException {
    synchronized (isWaitingForSerialization) {
      while (isSerializing.containsKey(name)) {
        isWaitingForSerialization.wait(100);
      }
    }
  }
}
