package org.enso.interpreter.runtime;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.nodes.InvalidAssumptionException;
import org.enso.interpreter.runtime.control.ThreadInterruptedException;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Phaser;
import java.util.concurrent.locks.ReentrantLock;

/** Manages threads running guest code, exposing a safepoint-like functionality. */
public class ThreadManager {
  private final Phaser safepointPhaser =
      new Phaser() {
        @Override
        protected boolean onAdvance(int phase, int registeredParties) {
          // Ensure the phaser never terminates, even if the number of parties drops to zero at some
          // point.
          return false;
        }
      };
  private final ReentrantLock lock = new ReentrantLock();

  private volatile boolean safepoint = false;
  private final ConcurrentHashMap<Thread, Boolean> interruptFlags = new ConcurrentHashMap<>();

  /**
   * Registers the current thread as running guest code.
   *
   * <p>From this point on, the thread is assumed to be controlled by the Enso runtime and e.g. will
   * be waited on in safepoints.
   *
   * <p>{@link #leave()} must be called immediately after guest execution is finished in the given
   * thread, otherwise a deadlock may occur.
   */
  public void enter() {
    safepointPhaser.register();
    interruptFlags.put(Thread.currentThread(), false);
  }

  /**
   * Deregisters the current thread from the control of the Enso runtime.
   *
   * <p>The thread may no longer execute Enso code, until {@link #enter()} is called again.
   */
  public void leave() {
    safepointPhaser.arriveAndDeregister();
    interruptFlags.remove(Thread.currentThread());
  }

  /** Called from the interpreter to periodically perform a safepoint check. */
  public void poll() {
    if (safepoint) {
      CompilerDirectives.transferToInterpreter();
      safepointPhaser.arriveAndAwaitAdvance();
      if (interruptFlags.get(Thread.currentThread())) {
        interruptFlags.put(Thread.currentThread(), false);
        throw new ThreadInterruptedException();
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
      enter();
      try {
        safepoint = true;
        safepointPhaser.arriveAndAwaitAdvance();
        safepoint = false;
      } finally {
        leave();
      }
    } finally {
      lock.unlock();
    }
  }
}
