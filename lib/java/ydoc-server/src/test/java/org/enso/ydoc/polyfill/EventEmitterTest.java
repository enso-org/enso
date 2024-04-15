package org.enso.ydoc.polyfill;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.HostAccess;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class EventEmitterTest {

  private Context context;
  private ExecutorService executor;

  public EventEmitterTest() {}

  @Before
  public void setup() throws Exception {
    executor = Executors.newSingleThreadExecutor();
    var eventTarget = new EventTarget(executor);
    var eventEmitter = new EventEmitter();

    var hostAccess = HostAccess.newBuilder(HostAccess.EXPLICIT).allowArrayAccess(true).build();
    var b = Context.newBuilder("js").allowHostAccess(hostAccess);

    var chromePort = Integer.getInteger("inspectPort", -1);
    if (chromePort > 0) {
      b.option("inspect", ":" + chromePort);
    }

    context =
        CompletableFuture.supplyAsync(
                () -> {
                  var ctx = b.build();
                  eventTarget.initialize(ctx);
                  eventEmitter.initialize(ctx);
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
  public void emit() throws Exception {
    var code =
        """
        var count = 0;
        var ee = new EventEmitter();
        ee.on('inc', (a, b) => count += 10*a + b);
        ee.emit('inc', 4, 2);
        count;
        """;

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    Assert.assertEquals(42, result.asInt());
  }

  @Test
  public void listeners() throws Exception {
    var code =
        """
        var count = 0;
        var ee = new EventEmitter();
        ee.on('inc', (a, b) => count += 10*a + b);
        ee.listeners('inc')
        """;

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();
    var arr = result.as(Object[].class);

    Assert.assertEquals(1, arr.length);
    Assert.assertNotNull(arr[0]);
  }
}
