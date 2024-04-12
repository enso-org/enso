package org.enso.ydoc.polyfill;

import io.helidon.webclient.websocket.WsClient;
import io.helidon.websocket.WsListener;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicReferenceArray;
import org.graalvm.polyglot.Context;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class WebSocketServerTest {

  private Context context;
  private ExecutorService executor;

  public WebSocketServerTest() {}

  @Before
  public void setup() throws Exception {
    executor = Executors.newSingleThreadExecutor();
    var b = Context.newBuilder("js").allowAllAccess(true);

    var chromePort = Integer.getInteger("inspectPort", -1);
    if (chromePort > 0) {
      b.option("inspect", ":" + chromePort);
    }

    context =
        CompletableFuture.supplyAsync(
                () -> {
                  var ctx = b.build();
                  Platform.initialize(ctx, executor);
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
  public void webSocketServer() throws Exception {
    var lock = new Semaphore(0);
    var res = new AtomicReferenceArray<>(new Object[2]);

    var code =
        """
        const onconnect = (ws, url) => {
            res.set(0, ws.readyState === WebSocket.OPEN);
            res.set(1, url);
            lock.release();
        }

        var wss = new WebSocketServer({host: 'localhost', port: 33445});
        wss.onconnect = onconnect;

        wss.start();
        """;

    context.getBindings("js").putMember("lock", lock);
    context.getBindings("js").putMember("res", res);

    CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    var ws = WsClient.builder().build();
    ws.connect("ws://localhost:33445/", new TestWsListener());

    lock.acquire();

    Assert.assertTrue((boolean) res.get(0));
    Assert.assertEquals("/", res.get(1));
  }

  private static final class TestWsListener implements WsListener {
    TestWsListener() {}
  }
}
