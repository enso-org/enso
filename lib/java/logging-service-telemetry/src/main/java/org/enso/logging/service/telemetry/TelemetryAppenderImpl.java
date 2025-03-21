package org.enso.logging.service.telemetry;

import ch.qos.logback.classic.spi.ILoggingEvent;
import java.io.IOException;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.enso.logging.service.logback.TelemetryAppender;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Background job processing inspired by {@code org.enso.base.enso_cloud.logging.LogApiAccess}.
 * Singleton. See {@link LogFormatter} for the expected format of log messages to this appender.
 *
 * <p>This appender is supposed to be started by the project manager, within {@link
 * org.enso.logging.service.logback.LoggingServer}. The logging events that are received in this
 * {@link TelemetryAppenderImpl#append(ILoggingEvent)} method are received from a socket and
 * deserialized by the logback framework. Thus, the {@link ILoggingEvent#getArgumentArray() log
 * event arguments} are most likely strings.
 */
@org.openide.util.lookup.ServiceProvider(service = TelemetryAppender.class)
public final class TelemetryAppenderImpl extends TelemetryAppender {
  private static final String CREDENTIALS_FILE_ENV = "ENSO_CLOUD_CREDENTIALS_FILE";

  /**
   * We still want to limit the batch size to some reasonable number - sending too many logs in one
   * request could also be problematic.
   */
  private static final int MAX_BATCH_SIZE = 100;

  private static final int MAX_RETRIES = 5;
  private static final Logger LOGGER = LoggerFactory.getLogger(TelemetryAppender.class.getName());

  private Credentials credentials;
  private final LogJobsQueue logQueue = new LogJobsQueue();

  private HttpClient httpClient;

  /**
   * Set to true once an error is encountered when sending a request. In such case, it is most
   * probably that no further requests will be successful. This flag is used to terminate the
   * background thread.
   */
  private boolean requestSendingFailure;

  @Override
  protected void append(ILoggingEvent eventObject) {
    enqueueJob(eventObject);
  }

  private static Path credentialsFile() {
    var env = System.getenv(CREDENTIALS_FILE_ENV);
    if (env != null) {
      return Path.of(env);
    }
    var home = Path.of(System.getProperty("user.home"));
    var credentials = home.resolve(".enso").resolve("credentials");
    return credentials;
  }

  private static Credentials parseCredentials(Path file) throws IOException {
    assert file.toFile().exists();
    return Credentials$.MODULE$.parseFromFile(file.toFile());
  }

  private void enqueueJob(ILoggingEvent logEvent) {
    int queuedJobs = logQueue.enqueue(logEvent);
    if (queuedJobs == 1 && backgroundThreadService.getQueue().isEmpty()) {
      // If we are the first message in the queue, we need to start the background thread.
      // It is possible that a job was already running, but adding a new one will not hurt - once
      // the queue is empty, the currently running job will finish and any additional jobs will also
      // terminate immediately.
      if (!requestSendingFailure) {
        backgroundThreadService.execute(this::logThreadEntryPoint);
      }
    }

    /*
     * Liveness is guaranteed, because the queue size always increments exactly by 1,
     * so `enqueue` returns 1 if and only if the queue was empty beforehand.
     *
     * If the queue was empty before adding a message, we always schedule a `logThreadEntryPoint` to run,
     * unless it was already pending on the job queue.
     *
     * Any running `logThreadEntryPoint` will not finish until the queue is empty.
     * So after every append, either a job is already running or scheduled to be run.
     */
  }

  /** Runs as long as there are any pending log messages queued and sends them in batches. */
  private void logThreadEntryPoint() {
    while (true) {
      List<ILoggingEvent> pendingMessages = logQueue.popEnqueuedJobs(MAX_BATCH_SIZE);
      if (pendingMessages.isEmpty()) {
        // If there are no more pending messages, we can stop the thread for now.
        // If during this teardown a new message is added, it will see no elements on `logQueue` and
        // thus,
        // `logQueue.enqueue` will return 1, thus ensuring that at least one new job is scheduled.
        return;
      }
      try {
        sendBatch(pendingMessages);
      } catch (RequestFailureException e) {
        LOGGER.warn("Stopping the Telemetry appender - requests cannot be send", e);
        requestSendingFailure = true;
        return;
      }
    }
  }

  /**
   * Sends a batch of log messages.
   *
   * <p>The batch must not be empty and all messages must share the same request config.
   */
  private void sendBatch(List<ILoggingEvent> batch) throws RequestFailureException {
    assert !batch.isEmpty() : "The batch must not be empty.";

    var request = buildRequest(batch);
    if (request == null) {
      LOGGER.warn("Failed to build request for log messages. Skipping {} messages", batch.size());
    } else {
      sendLogRequest(request, MAX_RETRIES);
    }
  }

  private HttpRequest buildRequest(List<ILoggingEvent> logEvents) throws RequestFailureException {
    if (credentials == null) {
      var credentialsFile = credentialsFile();
      if (!credentialsFile.toFile().exists()) {
        LOGGER.warn("Credentials file not found at '{}'. Will not send telemetry", credentialsFile);
        throw new RequestFailureException("Credentials file not found", null);
      }
      try {
        credentials = parseCredentials(credentialsFile);
      } catch (IOException e) {
        LOGGER.warn(
            "Failed to parse credentials from '{}'. Will not send telemetry", credentialsFile);
        throw new RequestFailureException("Failed to parse credentials", null);
      }
      assert credentials != null;
    }
    var payload = buildPayload(logEvents);
    if (payload != null) {
      return HttpRequest.newBuilder()
          .uri(endpoint)
          .header("Authorization", "Bearer " + credentials.accessToken())
          .POST(HttpRequest.BodyPublishers.ofString(payload, StandardCharsets.UTF_8))
          .build();
    } else {
      return null;
    }
  }

  /**
   * Transforms the given log events into JSON payloads.
   *
   * @return null if none of the log events could be transformed into a payload.
   */
  private String buildPayload(List<ILoggingEvent> logEvents) {
    var logs = new ArrayList<ApiMessage.Log>();
    for (var logEvent : logEvents) {
      var logMessage =
          new LogMessage(
              logEvent.getLoggerName(), logEvent.getMessage(), logEvent.getArgumentArray());
      var payloadForLogEvent = LogFormatter.transform(logMessage);
      if (payloadForLogEvent != null) {
        logs.add(payloadForLogEvent);
      }
    }
    if (logs.size() != logEvents.size()) {
      LOGGER.warn("Failed to build payload for some log events");
    }
    if (logs.isEmpty()) {
      return null;
    } else {
      var payload = ApiMessage.createPayload(logs);
      return ApiMessage.serializePayload(payload);
    }
  }

  private void sendLogRequest(HttpRequest request, int retryCount) throws RequestFailureException {
    assert request != null;
    try {
      try {
        if (httpClient == null) {
          httpClient = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.ALWAYS).build();
        }
        HttpResponse<String> response =
            httpClient.send(request, HttpResponse.BodyHandlers.ofString());
        if (response.statusCode() < 200 || response.statusCode() >= 300) {
          throw new RequestFailureException(
              "Unexpected status code: " + response.statusCode() + " " + response.body(), null);
        }
      } catch (IOException | InterruptedException e) {
        // Promote a checked exception to a runtime exception to simplify the code.
        var errorMessage = e.getMessage() != null ? e.getMessage() : e.toString();
        throw new RequestFailureException("Failed to send log messages: " + errorMessage, e);
      }
    } catch (RequestFailureException e) {
      if (retryCount < 0) {
        LOGGER.debug("Failed to send log messages after retrying", e);
        throw e;
      } else {
        LOGGER.debug("Exception when sending log messages. Retrying...", e);
        sendLogRequest(request, retryCount - 1);
      }
    }
  }

  private static final class RequestFailureException extends Exception {
    public RequestFailureException(String message, Throwable cause) {
      super(message, cause);
    }
  }
}
