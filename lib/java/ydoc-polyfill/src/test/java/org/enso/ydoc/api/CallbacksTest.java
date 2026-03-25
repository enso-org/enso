package org.enso.ydoc.api;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import org.enso.ydoc.polyfill.ExecutorSetup;
import org.enso.ydoc.polyfill.web.WebEnvironment;
import org.graalvm.polyglot.Context;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class CallbacksTest extends ExecutorSetup {

  private Context context;

  public CallbacksTest() {}

  public final class TestCallbacks implements YjsChannel.Server {

    private Consumer<YjsChannel> handler;

    public TestCallbacks(Consumer<YjsChannel> handler) {
      this.handler = handler;
    }

    @Override
    public void onConnect(YjsChannel channel) {
      this.handler.accept(channel);
    }
  }

  @Before
  public void setup() throws Exception {
    super.setup();

    var hostAccess =
        WebEnvironment.defaultHostAccess
            // allowImplementations is required to call methods on JS objects from Java,
            // i.e. to call `YjsChannel::send` in the `TestCallbacks::onConnect` method
            .allowImplementations(YjsChannel.class)
            // public access is required to recognize Java lambdas passed to
            // `YjsChannel::subscribe` method as JS functions.
            .allowPublicAccess(true)
            .allowAccess(TestCallbacks.class.getDeclaredMethod("onConnect", YjsChannel.class))
            .allowAccess(AtomicReference.class.getDeclaredMethod("set", Object.class))
            .build();
    var contextBuilder = WebEnvironment.createContext(hostAccess);

    context = CompletableFuture.supplyAsync(contextBuilder::build, executor).get();
  }

  @After
  public void tearDown() throws InterruptedException {
    super.tearDown();
    context.close();
  }

  @Test
  public void onConnectSend() throws Exception {
    var res = new AtomicReference<>();
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
    var res = new AtomicReference<>();
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
    var res = new AtomicReference<>();
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
