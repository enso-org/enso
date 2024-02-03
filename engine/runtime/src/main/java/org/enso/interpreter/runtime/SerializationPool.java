package org.enso.interpreter.runtime;

import static org.enso.interpreter.util.ScalaConversions.cons;
import static org.enso.interpreter.util.ScalaConversions.nil;

import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import org.enso.compiler.context.CompilerContext.Module;
import org.enso.editions.LibraryName;
import org.enso.pkg.QualifiedName;

final class SerializationPool {
  /** The maximum number of serialization threads allowed. */
  private static final int maximumThreadCount = 2;

  /** The number of threads at compiler start. */
  private static final int startingThreadCount = maximumThreadCount;

  /** The thread keep-alive time in seconds. */
  private static final long threadKeepalive = 3;

  /** The debug logging level. */
  private static final Level debugLogLevel = Level.FINE;

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
  private final Map<QualifiedName, Future<scala.Boolean>> isWaitingForSerialization =
      new ConcurrentHashMap<>();

  /** The thread pool that handles serialization. */
  private final ThreadPoolExecutor pool;

  SerializationPool(TruffleCompilerContext context) {
    this.context = context;
    this.pool =
        new ThreadPoolExecutor(
            startingThreadCount,
            maximumThreadCount,
            threadKeepalive,
            TimeUnit.SECONDS,
            new LinkedBlockingDeque<Runnable>(),
            context::createSystemThread);
  }

  /*
  <E, M > Future<Boolean> doSerialize(Cache<E, M> cache, E entry, boolean useGlobalCacheLocations) {
    context.saveCache(cache, entry, useGlobalCacheLocations);
    return null;
  }
  */

  void prestartAllCoreThreads() {}

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
        synchronized (isWaitingForSerialization) {
          waitingCount = isWaitingForSerialization.size();
        }
        var jobCount = waitingCount + isSerializing.size();
        context.logSerializationManager(
            debugLogLevel, "Waiting for #{0} serialization jobs to complete.", jobCount);

        // Bound the waiting loop
        int maxCount = 60;
        int counter = 0;
        while (this.hasJobsRemaining() && counter < maxCount) {
          counter += 1;
          Thread.sleep(1 * 1000);
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
      Thread.sleep(100);
      context.logSerializationManager(debugLogLevel, "Serialization manager has been shut down.");
    }
  }

  /**
   * Checks if the provided module is in the process of being serialized.
   *
   * @param module the module to check
   * @return `true` if `module` is currently being serialized, `false` otherwise
   */
  boolean isSerializingModule(QualifiedName module) {
    return isSerializing.containsKey(module);
  }

  boolean isSerializingLibrary(LibraryName library) {
    return isSerializing.containsKey(toQualifiedName(library));
  }

  boolean isWaitingForSerialization(QualifiedName name) {
    synchronized (isWaitingForSerialization) {
      return isWaitingForSerialization.containsKey(name);
    }
  }

  /**
   * Checks if the provided module is waiting for serialization.
   *
   * @param module the module to check
   * @return `true` if `module` is waiting for serialization, `false` otherwise
   */
  private boolean isWaitingForSerialization(Module module) {
    return isWaitingForSerialization(module.getName());
  }

  /**
   * Checks if the provided library's bindings are waiting for serialization.
   *
   * @param library the library to check
   * @return `true` if `library` is waiting for serialization, `false` otherwise
   */
  boolean abort(QualifiedName name) {
    synchronized (isWaitingForSerialization) {
      if (isWaitingForSerialization(name)) {
        var prev = isWaitingForSerialization.remove(name);
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

  /**
   * Requests that serialization of `module` be aborted.
   *
   * <p>If the module is already in the process of serialization it will not be aborted.
   *
   * @param module the module for which to abort serialization
   * @return `true` if serialization for `module` was aborted, `false` otherwise
   */
  boolean abort(Module module) {
    return abort(module.getName());
  }

  void startSerializing(QualifiedName name) {
    synchronized (isWaitingForSerialization) {
      isWaitingForSerialization.remove(name);
    }
    isSerializing.put(name, true);
  }

  /**
   * Sets the module described by `name` as finished with serialization.
   *
   * @param name the name of the module to set as having finished serialization
   */
  void finishSerializing(QualifiedName name) {
    isSerializing.remove(name);
  }

  Future<scala.Boolean> submitTask(
      Callable<scala.Boolean> task, boolean useThreadPool, QualifiedName key) {
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
            debugLogLevel, "Serialization task failed for [" + key + "].", e);
        return CompletableFuture.failedFuture(e);
      }
    }
  }

  void waitWhileSerializing(QualifiedName name) throws InterruptedException {
    while (isSerializing.containsKey(name)) {
      Thread.sleep(100);
    }
  }

  private static QualifiedName toQualifiedName(LibraryName libraryName) {
    var namespace = cons(libraryName.namespace(), nil());
    return new QualifiedName(namespace, libraryName.name());
  }
}
