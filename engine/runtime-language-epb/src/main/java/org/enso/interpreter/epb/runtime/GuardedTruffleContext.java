package org.enso.interpreter.epb.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleContext;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.nodes.Node;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Consumer;

/** Wraps a {@link TruffleContext} by providing an optional GIL functionality. */
public class GuardedTruffleContext {
  private final TruffleContext context;
  private final Lock lock;

  /**
   * Creates a new instance of this wrapper.
   *
   * @param context the Truffle context to wrap
   * @param isSingleThreaded whether or not the context should be accessed through a GIL.
   */
  public GuardedTruffleContext(TruffleContext context, boolean isSingleThreaded) {
    this.context = context;
    if (isSingleThreaded) {
      this.lock = new ReentrantLock();
    } else {
      this.lock = null;
    }
  }

  /**
   * Spawns new thread with associated {@code context}
   *
   * @param env environment to spawn the thread in
   * @param run code to execute in given TruffleContext
   */
  public Thread createThread(TruffleLanguage.Env env, Consumer<TruffleContext> run) {
    return env.createThread(() -> run.accept(context), context);
  }

  /**
   * Enters this context. If this wrapper is single threaded and the context is in use, this method
   * will block indefinitely until the context becomes available.
   *
   * <p>Any code following a call to this method should be executed in a try/finally block, with
   * {@link #leave(Node, Object)} being called in the finally block. It is crucial that this context
   * is always left as soon as guest code execution finishes.
   *
   * <p>The token returned from this method may not be stored or used for any purpose other than
   * leaving the context.
   *
   * @param node the node to enter this context for
   * @return a context restoration token that must be passed to {@link #leave(Node, Object)}
   */
  public Object enter(Node node) {
    if (lock != null) {
      lock();
    }
    return context.enter(node);
  }

  @CompilerDirectives.TruffleBoundary
  private void lock() {
    lock.lock();
  }

  /**
   * Leaves the context and unlocks it if this wrapper is GILed.
   *
   * @param node the node to leave this context for (this must correspond to the same node used in
   *     the call that provided the {@code prev} token
   * @param prev the token obtained from the call to {@link #enter(Node)}
   */
  public void leave(Node node, Object prev) {
    context.leave(node, prev);
    if (lock != null) {
      unlock();
    }
  }

  @CompilerDirectives.TruffleBoundary
  private void unlock() {
    lock.unlock();
  }
}
