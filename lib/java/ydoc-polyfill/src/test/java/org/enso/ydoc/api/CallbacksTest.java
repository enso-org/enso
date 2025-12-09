package org.enso.ydoc.api;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicReference;
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

  public final class TestCallbacks implements MessageCallbacks {

    private final AtomicReference<Object> ref;

    public TestCallbacks(AtomicReference<Object> ref) {
      this.ref = ref;
    }

    @Override
    public void onConnect(YjsChannel channel) {
      // allowImplementations(YjsChannel.class) host access is required to make this call
      channel.send("World!");
    }

    @Override
    public void onMessage(Object message) {
      ref.set(message);
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
            .allowAccess(TestCallbacks.class.getDeclaredMethod("onMessage", Object.class))
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
  public void onMessage() throws Exception {
    var res = new AtomicReference<>();
    var code =
        """
        callbacks.onMessage('Hello!');
        """;

    var callbacks = new TestCallbacks(res);
    context.getBindings("js").putMember("callbacks", callbacks);

    CompletableFuture.runAsync(() -> context.eval("js", code), executor).get();

    Assert.assertEquals("Hello!", res.get());
  }

  @Test
  public void onConnect() throws Exception {
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

    var callbacks = new TestCallbacks(null);
    context.getBindings("js").putMember("callbacks", callbacks);
    context.getBindings("js").putMember("res", res);

    CompletableFuture.runAsync(() -> context.eval("js", code), executor).get();

    Assert.assertEquals("World!", res.get());
  }
}
