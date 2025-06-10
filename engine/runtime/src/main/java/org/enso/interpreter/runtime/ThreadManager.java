package org.enso.interpreter.runtime;

import com.oracle.truffle.api.ThreadLocalAction;
import com.oracle.truffle.api.TruffleLanguage.Env;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.function.Supplier;
import org.enso.interpreter.runtime.control.ThreadInterruptedException;

/** Manages threads running guest code, exposing a safepoint-like functionality. */
public final class ThreadManager extends GuestCodeExecutor {
  private final ThreadExecutors threads;
  private final ConcurrentHashMap<Thread, Boolean> interruptFlags = new ConcurrentHashMap<>();

  ThreadManager(ThreadExecutors th, int throughput, Env env) {
    super(th.newScheduledThreadPool(throughput, "guest-code", false));
    this.threads = th;
  }

  @Override
  protected Runnable wrap(Runnable r) {
    Runnable wrap =
        () -> {
          var notYetEntered = interruptFlags.get(Thread.currentThread()) == null;
          if (notYetEntered) {
            interruptFlags.put(Thread.currentThread(), false);
            try {
              r.run();
            } finally {
              interruptFlags.remove(Thread.currentThread());
            }
          } else {
            r.run();
          }
        };
    return wrap;
  }

  @Override
  protected <V> Callable<V> wrap(Callable<V> c) {
    Callable<V> wrap =
        () -> {
          var notYetEntered = interruptFlags.get(Thread.currentThread()) == null;
          if (notYetEntered) {
            interruptFlags.put(Thread.currentThread(), false);
            try {
              return c.call();
            } finally {
              interruptFlags.remove(Thread.currentThread());
            }
          } else {
            return c.call();
          }
        };
    return wrap;
  }

  /**
   * Schedules a computation of provided action on one of available <em>guest threads</em>.
   *
   * @param <T> type of the value to operate on
   * @param action the action to perform to obtain a value
   * @return observable future filled with the computed value
   */
  public final <T> CompletableFuture<T> submit(Supplier<T> action) {
    var notYetEntered = interruptFlags.get(Thread.currentThread()) == null;
    if (notYetEntered) {
      Supplier<T> wrap =
          () -> {
            interruptFlags.put(Thread.currentThread(), false);
            try {
              return action.get();
            } finally {
              interruptFlags.remove(Thread.currentThread());
            }
          };
      return CompletableFuture.supplyAsync(wrap, guestCode);
    } else {
      try {
        return CompletableFuture.completedFuture(action.get());
      } catch (Exception ex) {
        return CompletableFuture.failedFuture(ex);
      }
    }
  }

  /**
   * Creates new cached pool of system threads associated with this context.
   *
   * @param name human-readable name of the pool
   * @param min minimal number of threads kept-alive in the pool
   * @param max maximal number of available threads
   * @param maxQueueSize maximal number of pending tasks
   * @return new execution service for this context
   */
  public ExecutorService newCachedThreadPool(String name, int min, int max, int maxQueueSize) {
    // only allow creation of systemThreads
    // non-system threads have to be managed and controlled internally
    return threads.newCachedThreadPool(name, true, min, max, maxQueueSize);
  }

  /**
   * Creates new fixed pool of system threads associated with this context.
   *
   * @param parallel amount of parallelism for the pool
   * @param name human-readable name of the pool
   * @return new execution service for this context
   */
  public ScheduledExecutorService newFixedThreadPool(int parallel, String name) {
    // only allow creation of systemThreads
    // non-system threads have to be managed and controlled internally
    return threads.newScheduledThreadPool(parallel, name, true);
  }

  /**
   * Forces all threads managed by this system to halt at the next safepoint (i.e. a {@link #poll()}
   * call) and throw a {@link ThreadInterruptedException}.
   *
   * <p>This method is blocking, does not return until the last managed thread reports at a
   * safepoint.
   *
   * <p>This method may not be called from a thread that is itself managed by this system, as doing
   * so may result in a deadlock.
   */
  public final void interruptThreads() {
    interruptFlags.replaceAll((t, b) -> true);
    submitThreadLocal(
        null,
        new ThreadLocalAction(true, false) {
          @Override
          protected void perform(ThreadLocalAction.Access access) {
            Boolean interrupt = interruptFlags.get(access.getThread());
            if (Boolean.TRUE.equals(interrupt)) {
              throw new ThreadInterruptedException();
            }
          }
        });
  }

  /** Requests that all threads are shutdown. */
  @Override
  public final void shutdown() {
    threads.shutdown();
    var hasBeenInterrupted = Thread.interrupted();
    for (var t : interruptFlags.keySet()) {
      try {
        t.join();
      } catch (InterruptedException e) {
        hasBeenInterrupted = true;
      }
    }
    if (hasBeenInterrupted) {
      Thread.currentThread().interrupt();
    }
  }

  /**
   * Invokes {@link Env#submitThreadLocal}.
   *
   * @param threads {@code null} or list of threads to execute action at
   * @param action the action to execute at given threads
   * @return future to check whether action has been executed
   */
  public final Future<Void> submitThreadLocal(Thread[] threads, ThreadLocalAction action) {
    return this.threads.submitThreadLocal(threads, action);
  }

  final Thread createThread(boolean systemThread, Runnable run) {
    return this.threads.createThread(systemThread, run);
  }
}
