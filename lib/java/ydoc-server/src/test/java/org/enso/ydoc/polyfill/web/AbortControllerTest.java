package org.enso.ydoc.polyfill.web;

import java.util.concurrent.CompletableFuture;
import org.enso.ydoc.polyfill.ExecutorSetup;
import org.graalvm.polyglot.Context;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class AbortControllerTest extends ExecutorSetup {

  private Context context;

  public AbortControllerTest() {}

  @Before
  public void setup() throws Exception {
    super.setup();
    var eventTarget = new EventTarget();
    var abortController = new AbortController();
    var contextBuilder = WebEnvironment.createContext();

    context =
        CompletableFuture.supplyAsync(
                () -> {
                  var ctx = contextBuilder.build();
                  eventTarget.initialize(ctx);
                  abortController.initialize(ctx);
                  return ctx;
                },
                executor)
            .get();
  }

  @After
  public void tearDown() throws InterruptedException {
    super.tearDown();
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
