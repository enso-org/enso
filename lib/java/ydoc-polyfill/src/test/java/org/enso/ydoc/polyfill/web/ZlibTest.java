package org.enso.ydoc.polyfill.web;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

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
  public void bufferToHexString() throws Exception {
    var code =
        """
        const buf = Buffer.from('hello world', 'utf8');
        // Prints: 68656c6c6f20776f726c64
        buf.toString('hex')
        """;

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    assertEquals("68656c6c6f20776f726c64", result.asString());
  }

  @Test
  public void bufferFromArrayNoArgs() throws Exception {
    var code =
        """
        const arr = new Uint16Array(2);

        arr[0] = 5000;
        arr[1] = 4000;

        // Shares memory with `arr`.
        const buf = Buffer.from(arr.buffer);

        const t1 = buf.toString("hex"); // Prints: <Buffer 88 13 a0 0f>

        // Changing the original Uint16Array changes the Buffer also.
        arr[1] = 6000;

        const t2 = buf.toString("hex"); // Prints: <Buffer 88 13 70 17>
        [buf, t1, t2]
        """;

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();
    assertTrue("It is an array", result.hasArrayElements());
    assertEquals("Has three elements", 3, result.getArraySize());

    var buf = result.getArrayElement(0);
    assertEquals("It is a buffer", "Buffer", buf.getMetaObject().getMetaSimpleName());

    Assert.assertEquals("8813a00f", result.getArrayElement(1).asString());
    Assert.assertEquals("88137017", result.getArrayElement(2).asString());
  }

  @Test
  public void bufferFromArrayOneArg() throws Exception {
    var code =
        """
        const arr = new Uint16Array(2);

        arr[0] = 5000;
        arr[1] = 4000;

        // Shares memory with `arr`.
        const buf = Buffer.from(arr.buffer, 1);

        const t1 = buf.toString("hex"); // Prints: <Buffer 13 a0 0f>

        // Changing the original Uint16Array changes the Buffer also.
        arr[1] = 6000;

        const t2 = buf.toString("hex"); // Prints: <Buffer 13 70 17>
        [buf, t1, t2]
        """;

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();
    assertTrue("It is an array", result.hasArrayElements());
    assertEquals("Has three elements", 3, result.getArraySize());

    var buf = result.getArrayElement(0);
    assertEquals("It is a buffer", "Buffer", buf.getMetaObject().getMetaSimpleName());

    Assert.assertEquals("13a00f", result.getArrayElement(1).asString());
    Assert.assertEquals("137017", result.getArrayElement(2).asString());
  }

  @Test
  public void bufferFromArrayTwoArgs() throws Exception {
    var code =
        """
        const arr = new Uint16Array(2);

        arr[0] = 5000;
        arr[1] = 4000;

        // Shares memory with `arr`.
        const buf = Buffer.from(arr.buffer, 1, 2);

        const t1 = buf.toString("hex"); // Prints: <Buffer 13 a0>

        // Changing the original Uint16Array changes the Buffer also.
        arr[1] = 6000;

        const t2 = buf.toString("hex"); // Prints: <Buffer 13 70>
        [buf, t1, t2]
        """;

    var result = CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();
    assertTrue("It is an array", result.hasArrayElements());
    assertEquals("Has three elements", 3, result.getArraySize());

    var buf = result.getArrayElement(0);
    assertEquals("It is a buffer", "Buffer", buf.getMetaObject().getMetaSimpleName());

    Assert.assertEquals("13a0", result.getArrayElement(1).asString());
    Assert.assertEquals("1370", result.getArrayElement(2).asString());
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
