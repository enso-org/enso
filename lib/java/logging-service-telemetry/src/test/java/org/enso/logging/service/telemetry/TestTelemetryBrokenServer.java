package org.enso.logging.service.telemetry;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.fail;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import org.enso.logging.service.common.AuthenticationData;
import org.enso.logging.service.common.LogJob;
import org.enso.logging.service.common.LogJobsProcessor;
import org.enso.logging.service.common.LogMessage;
import org.enso.logging.service.common.TokenRefresher;
import org.enso.testkit.RetryTestRule;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

public class TestTelemetryBrokenServer {
  @Rule public final ConsumeLogs consumeLogs = new ConsumeLogs();
  @Rule public final RetryTestRule retry = new RetryTestRule(3);

  private static final int port = 8098;
  private static final long APPENDER_KEEP_ALIVE = 20;
  private ThreadPoolExecutor logProcessorExecutor;
  private LogJobsProcessor logJobsProcessor;
  private TokenRefresher tokenRefresher;

  @Before
  public void before() {
    logProcessorExecutor =
        new ThreadPoolExecutor(
            0, 1, APPENDER_KEEP_ALIVE, TimeUnit.SECONDS, new LinkedBlockingQueue<>());
    var credentials = Utils.mockCredentials(port);
    tokenRefresher =
        new TokenRefresher(
            Utils.refreshUri(port), credentials.clientId(), credentials.refreshToken());
    var authData = AuthenticationData.fromCredentials(credentials);
    logJobsProcessor =
        new TelemetryLogJobsProcessor(
            logProcessorExecutor, Utils.logUri(port), authData, tokenRefresher, true);
  }

  @After
  public void after() {
    if (logProcessorExecutor != null) {
      logProcessorExecutor.shutdown();
    }
  }

  @Test
  public void continueSendingAfterFirstFailure() throws InterruptedException {
    var brokenServer = new BrokenServer();
    brokenServer.start();

    var message1 = new LogMessage("TestLogger", "msg: name={}", new Object[] {"Pavel"});
    var notification1 = new CompletableFuture<Void>();
    logJobsProcessor.enqueueMessage(new LogJob(message1, notification1));
    try {
      notification1.get();
      fail("First message should fail");
    } catch (ExecutionException e) {
      // nop - expected
    } catch (InterruptedException e) {
      throw new AssertionError("Should not reach here", e);
    }

    brokenServer.stop();
    brokenServer = null;

    // Give the broken server some time to properly shutdown
    Thread.sleep(30);
    var server = Utils.createMockServer(port);
    server.start();

    var message2 = new LogMessage("TestLogger", "msg2: name={}", new Object[] {"Pavel"});
    var job2 = new LogJob(message2, new CompletableFuture<>());
    logJobsProcessor.enqueueMessage(job2);
    Utils.assertCompletedSuccessfully(job2);

    var logs = server.getLogs();
    assertThat(logs.size(), is(1));

    server.stop();
  }

  private static final class BrokenServer {
    private final HttpServer server;

    private BrokenServer() {
      var address = new InetSocketAddress("localhost", port);
      try {
        server = HttpServer.create(address, 0);
      } catch (IOException e) {
        throw new AssertionError(e);
      }
    }

    void start() {
      var handler =
          new HttpHandler() {
            @Override
            public void handle(HttpExchange exchange) throws IOException {
              var bodyBytes = "Internal server error".getBytes();
              exchange.getResponseHeaders().add("Content-Type", "application/text");
              exchange.getResponseBody().write(bodyBytes);
              exchange.sendResponseHeaders(500, bodyBytes.length);
            }
          };
      server.createContext("/", handler);
      server.start();
    }

    void stop() {
      server.stop(0);
    }
  }
}
