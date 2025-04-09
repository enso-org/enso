package org.enso.logging.service.telemetry.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.stream.IntStream;
import org.enso.logging.service.telemetry.Credentials;
import org.enso.logging.service.telemetry.LogJob;
import org.enso.logging.service.telemetry.LogJobsProcessor;
import org.enso.logging.service.telemetry.LogMessage;
import org.enso.shttp.HTTPTestHelperServer;
import org.enso.shttp.HybridHTTPServer;
import org.enso.shttp.cloud_mock.CloudMockSetup;
import org.enso.testkit.RetryTestRule;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

public class TestTelemetry {
  @Rule public RetryTestRule retry = new RetryTestRule(3);

  private static final int port = 8083;
  private static final URI baseUri = URI.create("http://localhost:" + port + "/enso-cloud-mock");
  private static final URI logUri = URI.create(baseUri + "/logs");
  private static final URI refreshUri = URI.create(baseUri + "/enso-cloud-auth-renew");
  private static final long APPENDER_KEEP_ALIVE = 20;
  private static final Credentials credentials = mockCredentials();

  private HybridHTTPServer server;
  private ExecutorService serverExecutor;
  private ThreadPoolExecutor logProcessorExecutor;
  private LogJobsProcessor logJobsProcessor;

  @Before
  public void initServer() throws URISyntaxException, IOException {
    serverExecutor = Executors.newSingleThreadExecutor();
    logProcessorExecutor =
        new ThreadPoolExecutor(
            0, 1, APPENDER_KEEP_ALIVE, TimeUnit.SECONDS, new LinkedBlockingQueue<>());
    var cloudMockSetup = new CloudMockSetup(false);
    server =
        HTTPTestHelperServer.createServer("localhost", port, serverExecutor, false, cloudMockSetup);
    logJobsProcessor = new LogJobsProcessor(logProcessorExecutor, logUri, credentials);
    server.start();
  }

  @After
  public void stopServer() {
    server.stop();
    serverExecutor.shutdown();
    logProcessorExecutor.shutdown();
  }

  @Test
  public void sendSingleTelemetryLog() {
    var message = new LogMessage("TestLogger", "msg: name={}", new Object[] {"Pavel"});
    var notification = new CompletableFuture<Void>();
    logJobsProcessor.enqueueMessage(new LogJob(message, notification));
    try {
      notification.get();
    } catch (InterruptedException | ExecutionException e) {
      throw new AssertionError(e);
    }
    var receivedLogs = server.getLogs();
    assertThat(receivedLogs.size(), is(1));
    var receivedLog = receivedLogs.get(0);
    assertThat(receivedLog.projectId(), is(nullValue()));
    assertThat(receivedLog.message(), is("msg"));
    assertThat(receivedLog.metadata().get("name").asText(), is("Pavel"));
    assertThat(receivedLog.metadata().get("loggerName").asText(), is(message.loggerName()));
  }

  @Test
  public void incorrectlyFormattedMessage_ShouldNotBeSent() {
    var message =
        new LogMessage("TestLogger", "XX - incorrect format - XX", new Object[] {"Pavel"});
    var notification = new CompletableFuture<Void>();
    logJobsProcessor.enqueueMessage(new LogJob(message, notification));
    try {
      notification.get();
      fail("Should end exceptionally");
    } catch (ExecutionException e) {
      assertThat(e.getMessage(), containsString("Cannot build request"));
    } catch (InterruptedException e) {
      throw new AssertionError(e);
    }
    var receivedLogs = server.getLogs();
    assertThat(receivedLogs.isEmpty(), is(true));
  }

  private static Credentials mockCredentials() {
    var expireAt = ZonedDateTime.now().plusYears(1).format(DateTimeFormatter.ISO_INSTANT);
    var refreshUrl = refreshUri.toString();
    var accessToken = "TEST-ENSO-TOKEN-caffee";
    var refreshToken = "TEST-ENSO-REFRESH-caffee";
    var clientId = "TEST-ENSO-CLIENT-ID";
    return new Credentials(clientId, accessToken, refreshToken, refreshUrl, expireAt);
  }
}
