package org.enso.ydoc.polyfill.web;

import io.helidon.common.buffers.BufferData;
import io.helidon.webserver.WebServer;
import io.helidon.webserver.websocket.WsRouting;
import io.helidon.websocket.WsListener;
import io.helidon.websocket.WsSession;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.atomic.AtomicReferenceArray;
import org.enso.ydoc.polyfill.ExecutorSetup;
import org.graalvm.polyglot.Context;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class WebSocketTest extends ExecutorSetup {

  private Context context;
  private ExecutorService webServerExecutor;
  private WebServer ws;

  public WebSocketTest() {}

  private WebServer startWebSocketServer(ExecutorService executor) {
    var routing = WsRouting.builder().endpoint("/", new TestWsListener());
    var ws = WebServer.builder().host("localhost").port(22334).addRouting(routing).build();

    executor.submit(ws::start);

    return ws;
  }

  @Before
  public void setup() throws Exception {
    super.setup();
    webServerExecutor = Executors.newSingleThreadExecutor();
    ws = startWebSocketServer(webServerExecutor);

    var hostAccess =
        WebEnvironment.defaultHostAccess
            .allowAccess(AtomicBoolean.class.getDeclaredMethod("set", boolean.class))
            .allowAccess(AtomicReference.class.getDeclaredMethod("set", Object.class))
            .allowAccess(
                AtomicReferenceArray.class.getDeclaredMethod("set", int.class, Object.class))
            .allowAccess(Semaphore.class.getDeclaredMethod("acquire"))
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
    ws.stop();
    webServerExecutor.shutdown();
    super.tearDown();
    context.close();
  }

  @Test
  public void addEventListenerOpen() throws Exception {
    var lock = new Semaphore(0);
    var res = new AtomicBoolean(false);

    var code =
        """
        var ws = new WebSocket('ws://localhost:22334');
        ws.addEventListener('open', () => {
          res.set(true);
          lock.release();
        });
        """;

    context.getBindings("js").putMember("lock", lock);
    context.getBindings("js").putMember("res", res);

    CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    lock.acquire();

    Assert.assertTrue(res.get());
  }

  @Test
  public void dispatchClonedEvent() throws Exception {
    var lock = new Semaphore(0);
    var res = new AtomicReferenceArray<>(new Object[2]);

    var code =
        """
        var count = 0;
        var cloneEvent = (e) => new e.constructor(e.type, e);
        var ws = new WebSocket('ws://localhost:22334');
        var cb = (event) => {
          res.set(count, event.data);
          count += 1;
          if (count == 2) {
            ws.removeEventListener('message', cb);
          }
          ws.dispatchEvent(cloneEvent(event));
          lock.release();
        }
        ws.addEventListener('open', () => {
          ws.send('hello');
        });
        ws.addEventListener('message', cb);
        """;

    context.getBindings("js").putMember("lock", lock);
    context.getBindings("js").putMember("res", res);

    CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    lock.acquire(2);

    Assert.assertEquals("hello", res.get(0));
    Assert.assertEquals("hello", res.get(1));
  }

  @Test
  public void onOpen() throws Exception {
    var lock = new Semaphore(0);
    var res = new AtomicBoolean(false);

    var code =
        """
        var ws = new WebSocket('ws://localhost:22334');
        ws.on('open', () => {
          res.set(true);
          lock.release();
        });
        """;

    context.getBindings("js").putMember("lock", lock);
    context.getBindings("js").putMember("res", res);

    CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    lock.acquire();

    Assert.assertTrue(res.get());
  }

  @Test
  public void sendText() throws Exception {
    var lock = new Semaphore(0);
    var res = new AtomicReference<>();

    var code =
        """
        var ws = new WebSocket('ws://localhost:22334');
        ws.addEventListener('open', () => {
          ws.send('Hello!');
        });
        ws.onmessage = (event) => {
          res.set(event.data);
          lock.release();
        };
        """;

    context.getBindings("js").putMember("lock", lock);
    context.getBindings("js").putMember("res", res);

    CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    lock.acquire();

    Assert.assertEquals("Hello!", res.get());
  }

  @Test
  public void sendBinary() throws Exception {
    var lock = new Semaphore(0);
    var res = new AtomicReference<>();

    var code =
        """
        const ws = new WebSocket('ws://localhost:22334');
        ws.on('open', () => {
          ws.send(new Uint8Array([0, 31, 255]));
        });
        ws.on('message', (data) => {
          res.set(new Uint8Array(data).toString());
          lock.release();
        });
        """;

    context.getBindings("js").putMember("lock", lock);
    context.getBindings("js").putMember("res", res);

    CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    lock.acquire();

    Assert.assertEquals("0,31,255", res.get());
  }

  @Test
  public void ping() throws Exception {
    var lock = new Semaphore(0);
    var res = new AtomicBoolean(false);

    var code =
        """
        const ws = new WebSocket('ws://localhost:22334');
        ws.on('open', () => {
          ws.ping();
        });
        ws.on('pong', () => {
          res.set(true);
          lock.release();
        });
        """;

    context.getBindings("js").putMember("lock", lock);
    context.getBindings("js").putMember("res", res);

    CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    lock.acquire();

    Assert.assertTrue(res.get());
  }

  @Test
  public void closeIdempotent() throws Exception {
    var lock = new Semaphore(0);
    var res = new AtomicBoolean(false);

    var code =
        """
        var ws = new WebSocket('ws://localhost:22334');
        ws.addEventListener('open', () => {
          ws.close();
          ws.close();
          res.set(true);
          // Notify that the client socket is closed
          lock.release();
        });
        """;

    context.getBindings("js").putMember("lock", lock);
    context.getBindings("js").putMember("res", res);

    CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    // Wait until the client socket is closed
    lock.acquire();

    Assert.assertTrue(res.get());
  }

  @Test
  public void handleSocketClosed() throws Exception {
    var lock1 = new Semaphore(0);
    var lock2 = new Semaphore(0);
    var lock3 = new Semaphore(0);
    var res = new AtomicBoolean(false);

    var code =
        """
        var ws = new WebSocket('ws://localhost:22334');
        ws.addEventListener('open', () => {
          // Notify that the client socket is connected
          lock1.release();
          // Wait until the server socket is closed
          lock2.acquire();
          // Close the client socket (closed by the server)
          ws.close();
          res.set(true);
          // Notify that the client socket is closed
          lock3.release();
        });
        """;

    context.getBindings("js").putMember("lock1", lock1);
    context.getBindings("js").putMember("lock2", lock2);
    context.getBindings("js").putMember("lock3", lock3);
    context.getBindings("js").putMember("res", res);

    CompletableFuture.supplyAsync(() -> context.eval("js", code), executor).get();

    // Wait for the client socket connection
    lock1.acquire();
    // Close the server socket
    ws.stop();
    // Notify that the server socked is closed
    lock2.release();
    // Wait until the client socket is closed
    lock3.acquire();

    Assert.assertTrue(res.get());
  }

  private static final class TestWsListener implements WsListener {
    TestWsListener() {}

    @Override
    public void onMessage(WsSession session, String text, boolean last) {
      session.send(text, last);
    }

    @Override
    public void onMessage(WsSession session, BufferData buffer, boolean last) {
      session.send(buffer, last);
    }

    @Override
    public void onPing(WsSession session, BufferData buffer) {
      session.pong(buffer);
    }
  }
}
