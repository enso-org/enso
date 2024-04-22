package org.enso.ydoc.polyfill.nodejs;

import java.util.Arrays;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import org.graalvm.polyglot.Context;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class CryptoTest {

  private Context context;
  private ExecutorService executor;

  public CryptoTest() {}

  @Before
  public void setup() throws Exception {
    executor = Executors.newSingleThreadExecutor();
    var crypto = new Crypto();
    var b = Context.newBuilder("js");

    var chromePort = Integer.getInteger("inspectPort", -1);
    if (chromePort > 0) {
      b.option("inspect", ":" + chromePort);
    }

    context =
        CompletableFuture.supplyAsync(
                () -> {
                  var ctx = b.build();
                  crypto.initialize(ctx);
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
  public void sublte() throws Exception {
    var result =
        CompletableFuture.supplyAsync(() -> context.eval("js", "typeof crypto.subtle"), executor)
            .get();

    Assert.assertEquals("object", result.asString());
  }

  @Test
  public void cryptoRandomUUID() throws Exception {
    var result =
        CompletableFuture.supplyAsync(() -> context.eval("js", "crypto.randomUUID()"), executor)
            .get();

    Assert.assertNotNull(UUID.fromString(result.asString()));
  }

  @Test
  public void cryptoGetRandomValues() throws Exception {
    var code =
        """
        var arr = new Uint8Array(8);
        crypto.getRandomValues(arr);
        arr
        """;

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    Assert.assertTrue(Arrays.stream(result.as(int[].class)).anyMatch(i -> i > 0));
  }
}
