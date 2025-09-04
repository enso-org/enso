package org.enso.logging.service.telemetry;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.stream.IntStream;
import org.enso.logging.service.common.AuthenticationData;
import org.enso.logging.service.common.Credentials;
import org.enso.logging.service.common.LogJob;
import org.enso.logging.service.common.LogJobsProcessor;
import org.enso.logging.service.common.LogMessage;
import org.enso.logging.service.common.TokenRefresher;
import org.enso.shttp.HybridHTTPServer;
import org.enso.testkit.RetryTestRule;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

public class TestTelemetry {
  @Rule public final ConsumeLogs consumeLogs = new ConsumeLogs();
  @Rule public final RetryTestRule retry = new RetryTestRule(3);

  private static final int port = 8083;
  private static final URI logUri = Utils.logUri(port);
  private static final URI refreshUri = Utils.refreshUri(port);
  private static final long APPENDER_KEEP_ALIVE = 20;
  private static final Credentials credentials = Utils.mockCredentials(port);

  private HybridHTTPServer server;
  private ThreadPoolExecutor logProcessorExecutor;
  private LogJobsProcessor logJobsProcessor;
  private TokenRefresher tokenRefresher;

  @Before
  public void initServer() {
    logProcessorExecutor =
        new ThreadPoolExecutor(
            0, 1, APPENDER_KEEP_ALIVE, TimeUnit.SECONDS, new LinkedBlockingQueue<>());
    server = Utils.createMockServer(port);
    tokenRefresher =
        new TokenRefresher(refreshUri, credentials.clientId(), credentials.refreshToken());
    var authData = AuthenticationData.fromCredentials(credentials);
    logJobsProcessor =
        new TelemetryLogJobsProcessor(logProcessorExecutor, logUri, authData, tokenRefresher, true);
    server.start();
  }

  @After
  public void stopServer() {
    if (server != null) {
      server.stop();
    }
    if (logProcessorExecutor != null) {
      logProcessorExecutor.shutdown();
    }
  }

  @Test
  public void sendSingleTelemetryLog() {
    var message = new LogMessage("TestLogger", "msg: name={}", new Object[] {"Pavel"});
    var notification = new CompletableFuture<Void>();
    var job = new LogJob(message, notification);
    logJobsProcessor.enqueueMessage(job);
    Utils.assertCompletedSuccessfully(job);
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

  @Test
  public void sendMultipleTelemetryLogs() {
    var logJobs =
        IntStream.range(0, 5)
            .mapToObj(
                i -> {
                  var msg = new LogMessage("TestLogger-" + i, "msg: idx={}", new Object[] {i});
                  return new LogJob(msg, new CompletableFuture<>());
                })
            .toList();
    logJobs.forEach(logJobsProcessor::enqueueMessage);
    Utils.assertCompletedSuccessfully(logJobs);
    var logs = server.getLogs();
    for (int i = 0; i < logs.size(); i++) {
      var log = logs.get(i);
      assertThat(log.message(), is("msg"));
      assertThat(log.metadata().get("loggerName").asText(), containsString("TestLogger-" + i));
      assertThat(log.metadata().get("idx").asInt(), is(i));
    }
  }

  @Test
  public void failureToAuthenticate_InvalidToken() {
    var invalidCredentials = invalidCredentials();
    tokenRefresher =
        new TokenRefresher(
            refreshUri, invalidCredentials.clientId(), invalidCredentials.refreshToken());
    logJobsProcessor =
        new TelemetryLogJobsProcessor(
            logProcessorExecutor,
            logUri,
            AuthenticationData.fromCredentials(invalidCredentials),
            tokenRefresher,
            false);
    var message = new LogMessage("TestLogger", "msg: name={}", new Object[] {"Pavel"});
    var notification = new CompletableFuture<Void>();
    var job = new LogJob(message, notification);
    logJobsProcessor.enqueueMessage(job);
    try {
      notification.get();
      fail("Should end exceptionally");
    } catch (InterruptedException e) {
      throw new AssertionError("Should not be interrupted", e);
    } catch (ExecutionException e) {
      assertThat(e.getMessage(), containsString("401 Invalid token"));
    }
  }

  @Test
  public void refreshExpiredToken() {
    var expiredCredentials = expiredCredentials();
    tokenRefresher =
        new TokenRefresher(
            refreshUri, expiredCredentials.clientId(), expiredCredentials.refreshToken());
    logJobsProcessor =
        new TelemetryLogJobsProcessor(
            logProcessorExecutor,
            logUri,
            AuthenticationData.fromCredentials(expiredCredentials),
            tokenRefresher,
            true);

    // Ensure that the two messages are not sent in the batch - the two messages must be sent
    // in different requests so that we really check that the token was not refreshed twice.
    var message1 = new LogMessage("TestLogger", "msg: name={}", new Object[] {"Pavel"});
    var job1 = new LogJob(message1, new CompletableFuture<>());
    logJobsProcessor.enqueueMessage(job1);
    Utils.assertCompletedSuccessfully(job1);

    var job2 =
        new LogJob(
            new LogMessage("TestLogger", "msg2: name={}", new Object[] {"Pavel"}),
            new CompletableFuture<>());
    logJobsProcessor.enqueueMessage(job2);
    Utils.assertCompletedSuccessfully(job2);

    assertThat("Token was refreshed just once", server.getRefreshedTokensCount(), is(1));
  }

  private static Credentials invalidCredentials() {
    var expireAt = ZonedDateTime.now().plusYears(1).format(DateTimeFormatter.ISO_INSTANT);
    var refreshUrl = refreshUri.toString();
    var accessToken = "XX - wrong access token - XX";
    var refreshToken = "TEST-ENSO-REFRESH-caffee";
    var clientId = "TEST-ENSO-CLIENT-ID";
    return new Credentials(clientId, accessToken, refreshToken, refreshUrl, expireAt);
  }

  private Credentials expiredCredentials() {
    var expireAt = ZonedDateTime.now().minusMonths(10).format(DateTimeFormatter.ISO_INSTANT);
    var refreshUrl = refreshUri.toString();
    var accessToken = "TEST-EXPIRED-TOKEN-beef";
    var refreshToken = "TEST-ENSO-REFRESH-caffee";
    var clientId = "TEST-ENSO-CLIENT-ID";
    return new Credentials(clientId, accessToken, refreshToken, refreshUrl, expireAt);
  }
}
