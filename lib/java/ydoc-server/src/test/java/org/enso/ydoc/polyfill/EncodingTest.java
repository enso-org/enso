package org.enso.ydoc.polyfill;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import org.graalvm.polyglot.Context;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class EncodingTest {

  private Context context;
  private ExecutorService executor;

  public EncodingTest() {}

  @Before
  public void setup() throws Exception {
    executor = Executors.newSingleThreadExecutor();
    var encoding = new Encoding();

    var b = Context.newBuilder("js").allowExperimentalOptions(true);

    var chromePort = Integer.getInteger("inspectPort", -1);
    if (chromePort > 0) {
      b.option("inspect", ":" + chromePort);
    }

    context =
        CompletableFuture.supplyAsync(
                () -> {
                  var ctx = b.build();
                  encoding.initialize(ctx);
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
  public void textDecoderDecodeUtf8() throws Exception {
    var code =
        """
        let decoder = new TextDecoder();
        let arr = new Uint8Array([72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100, 33]);
        decoder.decode(arr);
        """;

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    Assert.assertEquals("Hello World!", result.as(String.class));
  }

  @Test
  public void textDecoderDecodeUtf8Explicit() throws Exception {
    var code =
        """
        let decoder = new TextDecoder('utf-8');
        let arr = new Uint8Array([72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100, 33]);
        decoder.decode(arr);
        """;

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    Assert.assertEquals("Hello World!", result.as(String.class));
  }

  @Test
  public void textDecoderDecodeWindows1251() throws Exception {
    var code =
        """
        let decoder = new TextDecoder('windows-1251');
        let arr = new Uint8Array([207, 240, 232, 226, 229, 242, 44, 32, 236, 232, 240, 33]);
        decoder.decode(arr);
        """;

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    Assert.assertEquals("Привет, мир!", result.as(String.class));
  }

  @Test
  public void textDecoderDecodeBufferWithOffset() throws Exception {
    var code =
        """
        let decoder = new TextDecoder();
        let arrHelloWorld = new Uint8Array([72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100, 33]);
        let arr = new Uint8Array(arrHelloWorld.buffer, 6, 5);
        decoder.decode(arr);
        """;

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    Assert.assertEquals("World", result.as(String.class));
  }
}
