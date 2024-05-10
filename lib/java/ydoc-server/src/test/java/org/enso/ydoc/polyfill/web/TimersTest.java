package org.enso.ydoc.polyfill.web;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import org.enso.ydoc.polyfill.ExecutorSetup;
import org.graalvm.polyglot.Context;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class TimersTest extends ExecutorSetup {

  private static final Consumer<Object> NULL_CONSUMER = v -> {};

  private Context context;

  public TimersTest() {}

  @Before
  public void setup() throws Exception {
    super.setup();
    var timers = new Timers(executor);

    var hostAccess =
        WebEnvironment.defaultHostAccess
            .allowAccess(Semaphore.class.getDeclaredMethod("release"))
            .build();
    var contextBuilder = WebEnvironment.createContext(hostAccess);

    context =
        CompletableFuture.supplyAsync(
                () -> {
                  var ctx = contextBuilder.build();
                  timers.initialize(ctx);
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
  public void setTimeout() throws Exception {
    var lock = new Semaphore(0);
    var code =
        """
        globalThis.result = 0;
        var p = function (x, y) {
            globalThis.result = 10*x + y;
            lock.release();
        };
        setTimeout(p, 0, 4, 2);
        """;

    context.getBindings("js").putMember("lock", lock);

    CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();
    lock.acquire();
    var result = CompletableFuture.supplyAsync(() -> context.eval("js", "result"), executor).get();

    Assert.assertEquals(42, result.asInt());
  }

  @Test
  public void clearTimeout() throws Exception {
    var code =
        """
        globalThis.result = 0;
        var p = function (x, y) {
            globalThis.result = 10*x + y;
        };
        var timeoutId = setTimeout(p, 9999, 4, 2);
        clearTimeout(timeoutId);
        """;

    var result =
        CompletableFuture.supplyAsync(() -> context.eval("js", code), executor)
            .thenApplyAsync(v -> context.eval("js", "result"), executor)
            .get();

    Assert.assertEquals(0, result.asInt());
  }

  @Test
  public void setInterval() throws Exception {
    var code =
        """
        globalThis.result = 0;
        var p = function (x, y) {
            globalThis.result += 10*x + y;
        };
        setInterval(p, 10, 4, 2);
        """;

    var result =
        CompletableFuture.supplyAsync(() -> context.eval("js", code), executor)
            .thenAcceptAsync(
                NULL_CONSUMER,
                CompletableFuture.delayedExecutor(500, TimeUnit.MILLISECONDS, executor))
            .thenApplyAsync(v -> context.eval("js", "result"), executor)
            .get();

    Assert.assertTrue(result.asInt() > 50);
  }

  @Test
  public void clearInterval() throws Exception {
    var code =
        """
        globalThis.result = 0;
        var p = function (x, y) {
            globalThis.result += 10*x + y;
        };
        var intervalId = setInterval(p, 10, 4, 2);
        clearInterval(intervalId);
        """;

    var result =
        CompletableFuture.supplyAsync(() -> context.eval("js", code), executor)
            .thenAcceptAsync(
                NULL_CONSUMER,
                CompletableFuture.delayedExecutor(500, TimeUnit.MILLISECONDS, executor))
            .thenApplyAsync(v -> context.eval("js", "result"), executor)
            .get();

    Assert.assertEquals(0, result.asInt());
  }
}
