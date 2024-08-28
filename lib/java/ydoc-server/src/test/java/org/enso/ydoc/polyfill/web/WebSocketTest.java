package org.enso.ydoc.polyfill.web;

import io.helidon.webserver.WebServer;
import io.helidon.webserver.websocket.WebSocketRouting;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.atomic.AtomicReferenceArray;

import jakarta.websocket.Endpoint;
import jakarta.websocket.EndpointConfig;
import jakarta.websocket.MessageHandler;
import jakarta.websocket.PongMessage;
import jakarta.websocket.Session;
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
    var routing = WebSocketRouting.builder().endpoint("/", TestWsListener.class);
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
    ws.shutdown();
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

  private static final class TestWsListener extends Endpoint {
    TestWsListener() {}

    @Override
    public void onOpen(Session session, EndpointConfig config) {
      session.addMessageHandler((MessageHandler.Whole<String>) message -> onMessage(session, message));
      session.addMessageHandler((MessageHandler.Whole<ByteBuffer>) message -> onMessage(session, message));
      session.addMessageHandler((MessageHandler.Whole<PongMessage>) message -> onPing(session, message.getApplicationData()));
    }

    private static void onMessage(Session session, String text) {
      try {
        session.getBasicRemote().sendText(text);
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
    }

    private static void onMessage(Session session, ByteBuffer buffer) {
      try {
        session.getBasicRemote().sendBinary(buffer);
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
    }

    private static void onPing(Session session, ByteBuffer buffer) {
      try {
        session.getBasicRemote().sendPong(buffer);
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
    }
  }
}
