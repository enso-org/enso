package org.enso.ydoc.polyfill;

import java.util.concurrent.CompletableFuture;
import org.enso.ydoc.polyfill.web.WebEnvironment;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.io.ByteSequence;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class ParserPolyfillTest extends ExecutorSetup {

  private Context context;
  private ParserPolyfill parser;

  public ParserPolyfillTest() {}

  @Before
  public void setup() throws Exception {
    super.setup();
    parser = new ParserPolyfill();
    var contextBuilder = WebEnvironment.createContext();

    context =
        CompletableFuture.supplyAsync(
                () -> {
                  var ctx = contextBuilder.build();
                  parser.initialize(ctx);
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
  public void parseModule() throws Exception {
    var code = """
        const arr = parse_module(`main = 1 + 2`)
        arr.buffer
        """;

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    Assert.assertTrue(result.as(ByteSequence.class).length() > 0);
  }

  @Test
  public void parseBlock() throws Exception {
    var code = """
        const arr = parse_block(`value = 1 + 2`)
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

    Assert.assertEquals("6930bc04", result.asString());
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
