package org.enso.ydoc.polyfill.web;

import java.util.concurrent.CompletableFuture;
import org.enso.ydoc.polyfill.ExecutorSetup;
import org.graalvm.polyglot.Context;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class ZlibTest extends ExecutorSetup {

  private static final String TEXT = "Hello World!";
  private static final String TEXT_BASE64 = "SGVsbG8gV29ybGQh";
  private static final String TEXT_DEFLATED = "eJzzSM3JyVcIzy/KSVEEABxJBD4=";

  private Context context;

  public ZlibTest() {}

  @Before
  public void setup() throws Exception {
    super.setup();
    var zlib = new Zlib();
    var contextBuilder = WebEnvironment.createContext();

    context =
        CompletableFuture.supplyAsync(
                () -> {
                  var ctx = contextBuilder.build();
                  zlib.initialize(eval(ctx));
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
  public void bufferFrom() throws Exception {
    var code = "Buffer.from(TEXT).toString()";

    context.getBindings("js").putMember("TEXT", TEXT);

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    Assert.assertEquals(TEXT, result.asString());
  }

  @Test
  public void bufferFromUtf8() throws Exception {
    var code = "Buffer.from(TEXT, 'utf8').toString()";

    context.getBindings("js").putMember("TEXT", TEXT);

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    Assert.assertEquals(TEXT, result.asString());
  }

  @Test
  public void bufferFromBase64() throws Exception {
    var code = "Buffer.from(TEXT_BASE64, 'base64').toString()";

    context.getBindings("js").putMember("TEXT_BASE64", TEXT_BASE64);

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    Assert.assertEquals(TEXT, result.asString());
  }

  @Test
  public void bufferFromInvalid() throws Exception {
    var code =
        """
        result = ''
        try {
          Buffer.from(TEXT, 'invalid').toString()
        } catch (e) {
          result = e.message
        }
        result
      """;

    context.getBindings("js").putMember("TEXT", TEXT);

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    Assert.assertEquals("Unknown encoding: invalid", result.asString());
  }

  @Test
  public void bufferToUtf8() throws Exception {
    var code = "Buffer.from(TEXT).toString('utf8')";

    context.getBindings("js").putMember("TEXT", TEXT);

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    Assert.assertEquals(TEXT, result.asString());
  }

  @Test
  public void bufferToBase64() throws Exception {
    var code = "Buffer.from(TEXT).toString('base64')";

    context.getBindings("js").putMember("TEXT", TEXT);

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    Assert.assertEquals(TEXT_BASE64, result.asString());
  }

  @Test
  public void bufferToInvalid() throws Exception {
    var code =
        """
        result = ''
        try {
          Buffer.from(TEXT).toString('invalid')
        } catch (e) {
          result = e.message
        }
        result
      """;

    context.getBindings("js").putMember("TEXT", TEXT);

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    Assert.assertEquals("Unknown encoding: invalid", result.asString());
  }

  @Test
  public void bufferToFromBase64() throws Exception {
    var code =
        """
        let textBase64 = Buffer.from(TEXT).toString('base64')
        Buffer.from(textBase64, 'base64').toString()
      """;

    context.getBindings("js").putMember("TEXT", TEXT);

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    Assert.assertEquals(TEXT, result.asString());
  }

  @Test
  public void zlibDeflateSync() throws Exception {
    var code =
        """
        let buffer = Buffer.from(TEXT)
        zlib.deflateSync(buffer).toString('base64')
      """;

    context.getBindings("js").putMember("TEXT", TEXT);

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    Assert.assertEquals(TEXT_DEFLATED, result.asString());
  }

  @Test
  public void zlibInflateSync() throws Exception {
    var code =
        """
        let buffer = Buffer.from(TEXT_DEFLATED, 'base64')
        zlib.inflateSync(buffer).toString()
      """;

    context.getBindings("js").putMember("TEXT_DEFLATED", TEXT_DEFLATED);

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    Assert.assertEquals(TEXT, result.asString());
  }

  @Test
  public void zlibDeflateInflate() throws Exception {
    var code =
        """
        let buffer = Buffer.from(TEXT)
        zlib.inflateSync(zlib.deflateSync(buffer)).toString()
      """;

    context.getBindings("js").putMember("TEXT", TEXT);

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    Assert.assertEquals(TEXT, result.asString());
  }

  @Test
  public void zlibInflateCorrupted() throws Exception {
    var code =
        """
        let buffer = Buffer.from('corrupted')
        let result = ''
        try {
          zlib.inflateSync(buffer).toString()
        } catch (e) {
          result = e.message
        }
        result
      """;

    context.getBindings("js").putMember("TEXT", TEXT);

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    Assert.assertEquals("Failed to inflate.", result.asString());
  }
}
