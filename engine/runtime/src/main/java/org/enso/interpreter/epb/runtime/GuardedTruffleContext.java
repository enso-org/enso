package org.enso.interpreter.epb.runtime;

import com.oracle.truffle.api.TruffleContext;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class GuardedTruffleContext {
  private final TruffleContext context;
  private final Lock lock;

  public GuardedTruffleContext(TruffleContext context, boolean isSingleThreaded) {
    this.context = context;
    if (isSingleThreaded) {
      this.lock = new ReentrantLock();
    } else {
      this.lock = null;
    }
  }

  public Object enter() {
    if (lock != null) {
      lock.lock();
    }
    return context.enter();
  }

  public void leave(Object prev) {
    if (lock != null) {
      lock.unlock();
    }
    context.leave(prev);
  }
}
