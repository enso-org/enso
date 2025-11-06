package org.enso.interpreter.runtime;

import com.oracle.truffle.api.ThreadLocalAction;
import com.oracle.truffle.api.TruffleLanguage.Env;
import com.oracle.truffle.api.TruffleLogger;
import java.util.Collections;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.concurrent.*;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Level;

/** Internal holder of all threads associated with {@link EnsoContext}. */
final class ThreadExecutors {
  private final TruffleLogger logger;
  private final Env env;
  private final Map<ExecutorService, String> pools =
      Collections.synchronizedMap(new WeakHashMap<>());
  private final Map<Thread, String> threads = Collections.synchronizedMap(new WeakHashMap<>());
  private int threadCounter;

  ThreadExecutors(Env env, TruffleLogger logger) {
    this.env = env;
    this.logger = logger;
  }

  ExecutorService newCachedThreadPool(
      String name, boolean systemThread, int min, int max, int maxQueueSize) {
    var s =
        new ThreadPoolExecutor(
            min,
            max,
            60L,
            TimeUnit.SECONDS,
            new LinkedBlockingQueue<>(maxQueueSize),
            new Factory(name, systemThread));
    pools.put(s, name);
    return s;
  }

  ScheduledExecutorService newScheduledThreadPool(int cnt, String name, boolean systemThread) {
    var s = new ScheduledThreadPoolExecutor(cnt, new Factory(name, systemThread));
    s.allowCoreThreadTimeOut(true);
    pools.put(s, name);
    return s;
  }

  public void shutdown() {
    synchronized (pools) {
      shutdownPools();
    }
    synchronized (threads) {
      for (var t : threads.keySet()) {
        try {
          t.join();
        } catch (InterruptedException ex) {
          logger.log(Level.WARNING, "Cannot shutdown {0} thread", t.getName());
        }
      }
    }
  }

  private void shutdownPools() {
    assert Thread.holdsLock(pools);
    var it = pools.entrySet().iterator();
    while (it.hasNext()) {
      var next = it.next();
      var p = next.getKey();
      p.shutdown();
      boolean success;
      try {
        success = p.awaitTermination(10, TimeUnit.SECONDS);
      } catch (InterruptedException ex) {
        success = false;
      }
      if (!success) {
        logger.log(Level.WARNING, "Cannot shutdown {0} thread pool", next.getValue());
      }
    }
  }

  /**
   * Invokes {@link Env#submitThreadLocal}.
   *
   * @param threads {@code null} or list of threads to execute action at
   * @param action the action to execute at given threads
   * @return future to check whether action has been executed
   */
  final Future<Void> submitThreadLocal(Thread[] threads, ThreadLocalAction action) {
    return env.submitThreadLocal(threads, action);
  }

  final Thread createThread(boolean systemThread, Runnable run) {
    if (systemThread) {
      var t = new Thread(run, "Enso thread #" + ++threadCounter);
      return t;
    } else {
      return env.newTruffleThreadBuilder(run).build();
    }
  }

  private final class Factory implements ThreadFactory {
    private final String prefix;
    private final AtomicInteger counter = new AtomicInteger(0);
    private final boolean system;

    Factory(String prefix, boolean systemThread) {
      this.prefix = prefix;
      this.system = systemThread;
    }

    @Override
    public Thread newThread(Runnable r) {
      var thread = createThread(system, r);
      thread.setName(prefix + "-" + counter.incrementAndGet());
      threads.put(thread, thread.getName());
      return thread;
    }
  }
}
