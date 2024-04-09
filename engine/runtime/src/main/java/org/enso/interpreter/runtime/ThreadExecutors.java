package org.enso.interpreter.runtime;

import java.util.Collections;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.concurrent.*;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Level;

final class ThreadExecutors {
  private final EnsoContext context;
  private final Map<ExecutorService, String> pools =
      Collections.synchronizedMap(new WeakHashMap<>());
  private final Map<Thread, String> threads = Collections.synchronizedMap(new WeakHashMap<>());

  ThreadExecutors(EnsoContext context) {
    this.context = context;
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

  ExecutorService newFixedThreadPool(int cnt, String name, boolean systemThread) {
    var s = Executors.newFixedThreadPool(cnt, new Factory(name, systemThread));
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
          context.getLogger().log(Level.WARNING, "Cannot shutdown {0} thread", t.getName());
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
        context.getLogger().log(Level.WARNING, "Cannot shutdown {0} thread pool", next.getValue());
      }
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
      var thread = context.createThread(system, r);
      thread.setName(prefix + "-" + counter.incrementAndGet());
      threads.put(thread, thread.getName());
      return thread;
    }
  }
}
