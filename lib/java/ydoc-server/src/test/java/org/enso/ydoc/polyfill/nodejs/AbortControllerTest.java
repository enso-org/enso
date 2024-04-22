package org.enso.ydoc.polyfill.nodejs;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import org.graalvm.polyglot.Context;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class AbortControllerTest {

  private Context context;
  private ExecutorService executor;

  public AbortControllerTest() {}

  @Before
  public void setup() throws Exception {
    executor = Executors.newSingleThreadExecutor();
    var eventTarget = new EventTarget(executor);
    var abortController = new AbortController();
    var b = Context.newBuilder("js");

    var chromePort = Integer.getInteger("inspectPort", -1);
    if (chromePort > 0) {
      b.option("inspect", ":" + chromePort);
    }

    context =
        CompletableFuture.supplyAsync(
                () -> {
                  var ctx = b.build();
                  eventTarget.initialize(ctx);
                  abortController.initialize(ctx);
                  return ctx;
                },
                executor)
            .get();
  }

  @After
  public void tearDown() {
    executor.close();
    context.close();
  }

  @Test
  public void getSignal() throws Exception {
    var code =
        """
        var ctrl = new AbortController();
        ctrl.signal.aborted;
        """;

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    Assert.assertFalse(result.asBoolean());
  }

  @Test
  public void abort() throws Exception {
    var code =
        """
        var ctrl = new AbortController();
        var signal = ctrl.signal;
        ctrl.abort();
        signal.aborted;
        """;

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    Assert.assertTrue(result.asBoolean());
  }
}
