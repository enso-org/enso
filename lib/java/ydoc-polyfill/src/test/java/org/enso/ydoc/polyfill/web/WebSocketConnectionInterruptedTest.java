package org.enso.ydoc.polyfill.web;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import io.helidon.webclient.websocket.WsClient;
import io.helidon.websocket.WsListener;
import io.helidon.websocket.WsSession;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.BlockingDeque;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import org.graalvm.polyglot.HostAccess;
import org.graalvm.polyglot.Value;
import org.junit.Test;

public class WebSocketConnectionInterruptedTest {

  @Test
  public void interruptThreadBeforeExecution() throws Exception {
    var mockExecutor = new MockExecutor();
    var ctx = WebEnvironment.createContext(HostAccess.ALL).build();
    WebEnvironment.initialize(ctx, mockExecutor);
    var server =
        """
        let ret = [ null ]
        let wss = new WebSocketServer({host: 'localhost', port: 33455});
        wss.onconnect = function() {
          ret[0] = "Successfully connected";
        };
        wss.start();
        ret
        """;
    var res = new Object[1];
    mockExecutor.execute(
        () -> {
          res[0] = ctx.eval("js", server);
        });

    {
      var cdl = new CountDownLatch(1);
      WsClient.builder()
          .build()
          .connect(
              "ws://localhost:33455",
              new WsListener() {
                @Override
                public void onOpen(WsSession session) {
                  cdl.countDown();
                }
              });
      cdl.await();
    }
    {
      var cdl = new CountDownLatch(1);
      mockExecutor.execute(
          () -> {
            assertNotNull("Result set", res[0]);
            assertTrue(res[0] instanceof Value);
            var v = (Value) res[0];
            assertTrue("It is an array", v.hasArrayElements());
            assertEquals(1, v.getArraySize());
            res[0] = v.getArrayElement(0).asString();
            cdl.countDown();
          });
      cdl.await();
      assertEquals("Successfully connected", res[0]);
    }
  }

  private static final class MockExecutor implements ScheduledExecutorService, Runnable {
    private final Thread thread;
    private final BlockingDeque<Runnable> run = new LinkedBlockingDeque<>();

    public MockExecutor() {
      thread = Thread.ofPlatform().name("mock executor").start(this);
    }

    @Override
    public void run() {
      for (; ; ) {
        Runnable r;
        try {
          r = run.takeFirst();
        } catch (InterruptedException ex) {
          ex.printStackTrace();
          continue;
        }
        r.run();
      }
    }

    @Override
    public void execute(Runnable command) {
      run.add(command);
    }

    @Override
    public <T> Future<T> submit(Callable<T> clbl) {
      var f = new CompletableFuture<T>();
      execute(
          () -> {
            try {
              var r = clbl.call();
              f.complete(r);
            } catch (Throwable t) {
              f.completeExceptionally(t);
            }
          });
      //
      // this generates an InterruptedException as soon as
      // someone calls `f.get()`
      //
      Thread.currentThread().interrupt();
      return f;
    }

    @Override
    public ScheduledFuture<?> schedule(Runnable r, long l, TimeUnit tu) {
      throw new UnsupportedOperationException();
    }

    @Override
    public <V> ScheduledFuture<V> schedule(Callable<V> clbl, long l, TimeUnit tu) {
      throw new UnsupportedOperationException();
    }

    @Override
    public ScheduledFuture<?> scheduleAtFixedRate(Runnable r, long l, long l1, TimeUnit tu) {
      throw new UnsupportedOperationException();
    }

    @Override
    public ScheduledFuture<?> scheduleWithFixedDelay(Runnable r, long l, long l1, TimeUnit tu) {
      throw new UnsupportedOperationException();
    }

    @Override
    public void shutdown() {
      throw new UnsupportedOperationException();
    }

    @Override
    public List<Runnable> shutdownNow() {
      throw new UnsupportedOperationException();
    }

    @Override
    public boolean isShutdown() {
      throw new UnsupportedOperationException();
    }

    @Override
    public boolean isTerminated() {
      throw new UnsupportedOperationException();
    }

    @Override
    public boolean awaitTermination(long l, TimeUnit tu) throws InterruptedException {
      throw new UnsupportedOperationException();
    }

    @Override
    public <T> Future<T> submit(Runnable r, T t) {
      throw new UnsupportedOperationException();
    }

    @Override
    public Future<?> submit(Runnable r) {
      throw new UnsupportedOperationException();
    }

    @Override
    public <T> List<Future<T>> invokeAll(Collection<? extends Callable<T>> tasks)
        throws InterruptedException {
      throw new UnsupportedOperationException();
    }

    @Override
    public <T> List<Future<T>> invokeAll(
        Collection<? extends Callable<T>> clctn, long l, TimeUnit tu) throws InterruptedException {
      throw new UnsupportedOperationException();
    }

    @Override
    public <T> T invokeAny(Collection<? extends Callable<T>> tasks)
        throws InterruptedException, ExecutionException {
      throw new UnsupportedOperationException();
    }

    @Override
    public <T> T invokeAny(Collection<? extends Callable<T>> clctn, long l, TimeUnit tu)
        throws InterruptedException, ExecutionException, TimeoutException {
      throw new UnsupportedOperationException();
    }
  }
}
