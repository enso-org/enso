package org.enso.ydoc.api;

import java.util.concurrent.CompletableFuture;
import java.util.function.Consumer;
import org.enso.ydoc.polyfill.ExecutorSetup;
import org.enso.ydoc.polyfill.web.WebEnvironment;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.HostAccess;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class CallbacksTest extends ExecutorSetup {

  private Context context;

  public CallbacksTest() {}

  @HostAccess.Implementable
  public interface JsYjsChannel {
    public void send(Object msg);

    public void subscribe(Consumer<Object> onMessage);
  }

  public final class TestCallbacks {
    private Consumer<YjsChannel<Object>> handler;

    TestCallbacks(Consumer<YjsChannel<Object>> handler) {
      this.handler = handler;
    }

    @HostAccess.Export
    public void onConnect(JsYjsChannel channel) {
      var wrap = YjsChannel.create(channel::send, channel::subscribe);
      this.handler.accept(wrap);
    }
  }

  @Before
  @Override
  public void setup() throws Exception {
    super.setup();

    var hostAccess = WebEnvironment.defaultHostAccess.allowPublicAccess(true).build();
    var contextBuilder = WebEnvironment.createContext(hostAccess);

    context = CompletableFuture.supplyAsync(contextBuilder::build, executor).get();
  }

  @After
  @Override
  public void tearDown() throws InterruptedException {
    super.tearDown();
    context.close();
  }

  public static final class JsRef {
    Object value;

    @HostAccess.Export
    public void set(Object v) {
      this.value = v;
    }

    Object get() {
      return this.value;
    }
  }

  @Test
  public void onConnectSend() throws Exception {
    var res = new JsRef();
    var code =
        """
        class YjsChannel {
          send(message) {
            res.set(message);
          }
        }

        var channel = new YjsChannel();
        callbacks.onConnect(channel);
        """;

    var callbacks = new TestCallbacks((channel) -> channel.send("Hello!"));
    context.getBindings("js").putMember("callbacks", callbacks);
    context.getBindings("js").putMember("res", res);

    CompletableFuture.runAsync(() -> context.eval("js", code), executor).get();

    Assert.assertEquals("Hello!", res.get());
  }

  @Test
  public void onConnectSubscribe() throws Exception {
    var res = new JsRef();
    var code =
        """
        class YjsChannel {
          subscribe(messageHandler) {
            messageHandler('World!');
          }
        }

        var channel = new YjsChannel();
        callbacks.onConnect(channel);
        """;

    var callbacks =
        new TestCallbacks((channel) -> channel.subscribe((message) -> res.set(message)));
    context.getBindings("js").putMember("callbacks", callbacks);

    CompletableFuture.runAsync(() -> context.eval("js", code), executor).get();

    Assert.assertEquals("World!", res.get());
  }

  @Test
  public void onConnectSubscribeBuffer() throws Exception {
    var res = new JsRef();
    var code =
        """
        class YjsChannel {
          subscribe(messageHandler) {
            var arr = new Uint8Array([0, 128, 255]);
            messageHandler(arr.buffer);
          }
        }

        var channel = new YjsChannel();
        callbacks.onConnect(channel);
        """;

    var callbacks =
        new TestCallbacks((channel) -> channel.subscribe((message) -> res.set(message)));
    context.getBindings("js").putMember("callbacks", callbacks);

    CompletableFuture.runAsync(() -> context.eval("js", code), executor).get();
    var value = context.asValue(res.get());
    var arr = value.as(byte[].class);

    Assert.assertArrayEquals(new byte[] {0, -128, -1}, arr);
  }
}
