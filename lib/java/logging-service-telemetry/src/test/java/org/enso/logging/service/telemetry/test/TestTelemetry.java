package org.enso.logging.service.telemetry.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executor;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import org.enso.logging.service.telemetry.Credentials;
import org.enso.logging.service.telemetry.LogJobsProcessor;
import org.enso.logging.service.telemetry.LogMessage;
import org.enso.shttp.HTTPTestHelperServer;
import org.enso.shttp.HybridHTTPServer;
import org.enso.shttp.cloud_mock.CloudMockSetup;
import org.enso.testkit.RetryTestRule;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;

public class TestTelemetry {
  private static final int port = 8083;
  private static final URI baseUri = URI.create("http://localhost:" + port + "/enso-cloud-mock");
  private static final URI logUri = URI.create(baseUri + "/logs");
  private static final URI refreshUri = URI.create(baseUri + "/enso-cloud-auth-renew");
  private static final URI getLogsUri = URI.create(baseUri + "/log_events");
  private static final long APPENDER_KEEP_ALIVE = 20;

  private static HybridHTTPServer server;
  private static MockServerExecutor serverExecutor;
  private static ThreadPoolExecutor logProcessorExecutor;
  private static LogJobsProcessor logJobsProcessor;
  private static Credentials credentials;

  @Rule public RetryTestRule retry = new RetryTestRule(3);

  @BeforeClass
  public static void initServer() throws URISyntaxException, IOException {
    serverExecutor = new MockServerExecutor();
    logProcessorExecutor =
        new ThreadPoolExecutor(
            0, 1, APPENDER_KEEP_ALIVE, TimeUnit.SECONDS, new LinkedBlockingQueue<>());
    var cloudMockSetup = new CloudMockSetup(false);
    server =
        HTTPTestHelperServer.createServer("localhost", port, serverExecutor, false, cloudMockSetup);
    credentials = mockCredentials();
    logJobsProcessor = new LogJobsProcessor(logProcessorExecutor, logUri, credentials);
    server.start();
  }

  @AfterClass
  public static void stopServer() {
    server.stop();
    server = null;
    serverExecutor.shutdown();
    serverExecutor = null;
    logProcessorExecutor.shutdown();
    logProcessorExecutor = null;
    logJobsProcessor = null;
    credentials = null;
  }

  @Test
  public void sendSingleTelemetryLog() throws InterruptedException, IOException {
    var message = new LogMessage("TestLogger", "msg: name={}", new Object[] {"Pavel"});
    logJobsProcessor.enqueueMessage(message);
    serverExecutor.waitForAllTasks();
    var receivedLogs = getLogs();
    assertThat(receivedLogs.size(), is(1));
    var receivedLog = receivedLogs.get(0);
    assertThat(receivedLog.projectId, is(nullValue()));
    assertThat(receivedLog.message, is("msg"));
    assertThat(receivedLog.metadata.get("name"), is("Pavel"));
    assertThat(receivedLog.metadata.get("loggerName"), is(message.loggerName()));
  }

  private static Credentials mockCredentials() {
    var expireAt = ZonedDateTime.now().plusYears(1).format(DateTimeFormatter.ISO_INSTANT);
    var refreshUrl = refreshUri.toString();
    var accessToken = "TEST-ENSO-TOKEN-caffee";
    var refreshToken = "TEST-ENSO-REFRESH-caffee";
    var clientId = "TEST-ENSO-CLIENT-ID";
    return new Credentials(clientId, accessToken, refreshToken, refreshUrl, expireAt);
  }

  /**
   * Fetches log events from the mock server. See {@code org.enso.shttp.cloud_mock.EventsService}.
   */
  private List<ReceivedLogEvent> getLogs() throws IOException, InterruptedException {
    var blockingExecutor = new BlockingExecutor();
    var httpClient = HttpClient.newBuilder().executor(blockingExecutor).build();
    var req =
        HttpRequest.newBuilder(getLogsUri)
            .header("Authorization", "Bearer " + credentials.accessToken())
            .GET()
            .build();
    var resp = httpClient.send(req, HttpResponse.BodyHandlers.ofString());
    assertThat(resp.statusCode(), is(200));
    var mapper = new ObjectMapper();
    var tree = mapper.readTree(resp.body());
    var events = tree.get("events");
    var receivedLogs = new ArrayList<ReceivedLogEvent>();
    events.forEach(
        event -> {
          try {
            var receivedLog = mapper.readValue(event.toString(), ReceivedLogEvent.class);
            receivedLogs.add(receivedLog);
          } catch (JsonProcessingException e) {
            throw new AssertionError(e);
          }
        });
    return receivedLogs;
  }

  private static final class BlockingExecutor implements Executor {
    @Override
    public void execute(Runnable command) {
      command.run();
    }
  }

  private static final class MockServerExecutor implements Executor {
    private final ExecutorService underlyingExecutor = Executors.newSingleThreadExecutor();
    private final List<Future<?>> tasks = new ArrayList<>();

    @Override
    public void execute(Runnable command) {
      var task = underlyingExecutor.submit(command);
      synchronized (tasks) {
        tasks.add(task);
        tasks.notifyAll();
      }
    }

    public void waitForAllTasks() {
      synchronized (tasks) {
        // There needs to be at least an initial task added to the list.
        // If the list is empty, no work has yet been started.
        while (tasks.isEmpty()) {
          try {
            tasks.wait();
          } catch (InterruptedException e) {
            throw new AssertionError(e);
          }
        }
      }
      for (var task : tasks) {
        try {
          task.get();
        } catch (InterruptedException | ExecutionException e) {
          throw new AssertionError(e);
        }
      }
    }

    void shutdown() {
      underlyingExecutor.shutdown();
      try {
        if (!underlyingExecutor.awaitTermination(5, TimeUnit.SECONDS)) {
          underlyingExecutor.shutdownNow();
        }
      } catch (InterruptedException e) {
        underlyingExecutor.shutdownNow();
      }
    }
  }

  private record ReceivedLogEvent(
      String organizationId,
      String userEmail,
      String timestamp,
      String message,
      String projectId,
      Map<String, String> metadata) {}
}
