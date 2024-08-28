package org.enso.ydoc.polyfill.web;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.WebSocket;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicReferenceArray;
import org.enso.ydoc.polyfill.ExecutorSetup;
import org.graalvm.polyglot.Context;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class WebSocketServerTest extends ExecutorSetup {

  private Context context;

  public WebSocketServerTest() {}

  @Before
  public void setup() throws Exception {
    super.setup();

    var hostAccess =
        WebEnvironment.defaultHostAccess
            .allowAccess(
                AtomicReferenceArray.class.getDeclaredMethod("set", int.class, Object.class))
            .allowAccess(Semaphore.class.getDeclaredMethod("release"))
            .build();
    var contextBuilder = WebEnvironment.createContext(hostAccess);

    context =
        CompletableFuture.supplyAsync(
                () -> {
                  var ctx = contextBuilder.build();
                  WebEnvironment.initialize(ctx, executor);
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
  public void webSocketServer() throws Exception {
    var lock = new Semaphore(0);
    var res = new AtomicReferenceArray<>(new Object[3]);

    var code =
        """
        const onconnect = (ws, url) => {
            res.set(0, ws.readyState === WebSocket.CONNECTING);
            res.set(1, url.pathname);
            res.set(2, url.searchParams.toString());
            lock.release();
        }

        let wss = new WebSocketServer({host: 'localhost', port: 33445});
        wss.onconnect = onconnect;

        wss.start();
        """;

    context.getBindings("js").putMember("lock", lock);
    context.getBindings("js").putMember("res", res);

    CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    //var ws = WsClient.builder().build();
    //ws.connect("ws://localhost:33445/hello?foo=bar", new TestWsListener());
    HttpClient.newHttpClient().newWebSocketBuilder().buildAsync(URI.create("ws://localhost:33445/hello?foo=bar"), new TestWsListener()).get();

    lock.acquire();

    Assert.assertTrue((boolean) res.get(0));
    Assert.assertEquals("/hello", res.get(1));
    Assert.assertEquals("foo=bar", res.get(2));
  }

  private static final class TestWsListener implements WebSocket.Listener {
    TestWsListener() {}
  }
}
