package org.enso.ydoc.polyfill;

import static org.junit.Assert.fail;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import org.junit.After;
import org.junit.Before;

public abstract class ExecutorSetup {

  protected ExecutorService executor;

  @Before
  public void setup() throws Exception {
    executor = Executors.newSingleThreadExecutor();
  }

  @After
  public void tearDown() throws InterruptedException {
    if (executor != null) {
      executor.shutdown();
      var stopped = executor.awaitTermination(3, TimeUnit.SECONDS);
      if (!stopped) {
        var pending = executor.shutdownNow();
        fail("Pending " + pending.size() + " tasks: " + pending);
      }
    }
  }
}
