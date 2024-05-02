package org.enso.ydoc.polyfill.web;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import org.graalvm.polyglot.Context;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class PerformanceTest {

  private Context context;
  private ExecutorService executor;

  public PerformanceTest() {}

  @Before
  public void setup() throws Exception {
    executor = Executors.newSingleThreadExecutor();
    var eventTarget = new Performance();
    var contextBuilder = WebEnvironment.createContext();

    context =
        CompletableFuture.supplyAsync(
                () -> {
                  var ctx = contextBuilder.build();
                  eventTarget.initialize(ctx);
                  return ctx;
                },
                executor)
            .get();
  }

  @After
  public void tearDown() {
    executor.shutdownNow();
    context.close();
  }

  @Test
  public void now() throws Exception {
    var result =
        CompletableFuture.supplyAsync(() -> context.eval("js", "performance.now()"), executor)
            .get();

    Assert.assertTrue(result.asLong() > 0);
    Assert.assertTrue(result.asLong() <= System.currentTimeMillis());
  }
}
