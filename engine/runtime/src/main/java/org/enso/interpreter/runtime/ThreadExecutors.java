package org.enso.interpreter.runtime;

import java.util.Map;
import java.util.WeakHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;

final class ThreadExecutors {
  private final EnsoContext context;
  private final Map<ExecutorService, String> pools = new WeakHashMap<>();

  ThreadExecutors(EnsoContext context) {
    this.context = context;
  }

  final ExecutorService newCachedThreadPool(String name, boolean systemThread) {
    var s = Executors.newCachedThreadPool(new Factory(name, systemThread));
    pools.put(s, name);
    return s;
  }

  final ExecutorService newFixedThreadPool(int cnt, String name, boolean systemThread) {
    var s = Executors.newFixedThreadPool(cnt, new Factory(name, systemThread));
    pools.put(s, name);
    return s;
  }

  public void shutdown() {
    var it = pools.entrySet().iterator();
    while (it.hasNext()) {
      var p = it.next().getKey();
      p.shutdown();
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
      var thread =
          system
              ? context.getEnvironment().createSystemThread(r)
              : context.getEnvironment().createThread(r);
      thread.setName(prefix + "-" + counter.incrementAndGet());
      return thread;
    }
  }
}
