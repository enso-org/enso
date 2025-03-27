package org.enso.interpreter.runtime;

import com.oracle.truffle.api.ThreadLocalAction;
import com.oracle.truffle.api.TruffleLanguage.Env;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Supplier;
import org.enso.interpreter.runtime.control.ThreadInterruptedException;

/** Manages threads running guest code, exposing a safepoint-like functionality. */
public final class ThreadManager {
  private final ReentrantLock lock = new ReentrantLock();
  private final Env env;

  private final ExecutorService questCode;
  private final ConcurrentHashMap<Thread, Boolean> interruptFlags = new ConcurrentHashMap<>();
  private static final Object ENTERED = new Object();
  private static final Object NO_OP = new Object();

  ThreadManager(ThreadExecutors th, Env env) {
    this.env = env;
    this.questCode = th.newCachedThreadPool("guest-code", false, 1, 4, 10);
  }

  /**
   * Registers the current thread as running guest code.
   *
   * <p>From this point on, the thread is assumed to be controlled by the Enso runtime and e.g. will
   * be waited on in safepoints.
   *
   * <p>{@link #leave(Object)} must be called immediately after guest execution is finished in the
   * given thread, otherwise a deadlock may occur.
   *
   * @return a token object that must be passed to {@link #leave(Object)}. This object is opaque,
   *     with no guarantees on its structure.
   */
  private Object enter() {
    if (isNotEnteredYet()) {
      interruptFlags.put(Thread.currentThread(), false);
      return ENTERED;
    }
    return NO_OP;
  }

  private boolean isNotEnteredYet() {
    return interruptFlags.get(Thread.currentThread()) == null;
  }

  /**
   * Deregisters the current thread from the control of the Enso runtime.
   *
   * <p>The thread may no longer execute Enso code, until {@link #enter()} is called again.
   *
   * @param token the token returned by the corresponding call to {@link #enter()}.
   */
  private void leave(Object token) {
    if (token != NO_OP) {
      interruptFlags.remove(Thread.currentThread());
    }
  }

  /**
   * Schedules a computation of provided action on one of available <em>guest threads</em>.
   *
   * @param <T> type of the value to operate on
   * @param action the action to perform to obtain a value
   * @return observable future filled with the computed value
   */
  public final <T> CompletableFuture<T> submit(Supplier<T> action) {
    if (isNotEnteredYet()) {
      Supplier<T> wrap =
          () -> {
            var tok = enter();
            try {
              return action.get();
            } finally {
              leave(tok);
            }
          };
      return CompletableFuture.supplyAsync(wrap, questCode);
    } else {
      try {
        return CompletableFuture.completedFuture(action.get());
      } catch (Exception ex) {
        return CompletableFuture.failedFuture(ex);
      }
    }
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
  public void interruptThreads() {
    lock.lock();
    try {
      interruptFlags.replaceAll((t, b) -> true);
      Object p = enter();
      try {
        env.submitThreadLocal(
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
      } finally {
        leave(p);
      }
    } finally {
      lock.unlock();
    }
  }

  /** Requests that all threads are shutdown. */
  public void shutdown() {
    var threads = interruptFlags.keySet();
    threads.forEach(
        t -> {
          try {
            t.join();
          } catch (InterruptedException e) {
            e.printStackTrace();
          }
        });
  }
}
