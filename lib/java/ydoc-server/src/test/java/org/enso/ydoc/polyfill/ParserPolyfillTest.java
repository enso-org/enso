package org.enso.ydoc.polyfill;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.HostAccess;
import org.graalvm.polyglot.io.ByteSequence;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class ParserPolyfillTest {

  private Context context;
  private ExecutorService executor;
  private ParserPolyfill parser;

  public ParserPolyfillTest() {}

  @Before
  public void setup() throws Exception {
    executor = Executors.newSingleThreadExecutor();
    parser = new ParserPolyfill();

    var hostAccess =
        HostAccess.newBuilder(HostAccess.EXPLICIT)
            .allowArrayAccess(true)
            .allowBufferAccess(true)
            .build();
    var b = Context.newBuilder("js").allowHostAccess(hostAccess).allowExperimentalOptions(true);

    var chromePort = Integer.getInteger("inspectPort", -1);
    if (chromePort > 0) {
      b.option("inspect", ":" + chromePort);
    }

    context =
        CompletableFuture.supplyAsync(
                () -> {
                  var ctx = b.build();
                  parser.initialize(ctx);
                  return ctx;
                },
                executor)
            .get();
  }

  @After
  public void tearDown() {
    executor.close();
    context.close();
    parser.close();
  }

  @Test
  public void parseTree() throws Exception {
    var code = """
        const arr = parse_tree(`main = 1 + 2`)
        arr.buffer
        """;

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    Assert.assertTrue(result.as(ByteSequence.class).length() > 0);
  }

  @Test
  public void xxHash128() throws Exception {
    var code = """
        xxHash128(`main = 1 + 2`)
        """;

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    Assert.assertEquals("1764801540", result.asString());
  }

  @Test
  public void isIdentOrOperator() throws Exception {
    var code = """
        is_ident_or_operator(`ident`)
        """;

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    Assert.assertEquals(1, result.asLong());
  }
}
