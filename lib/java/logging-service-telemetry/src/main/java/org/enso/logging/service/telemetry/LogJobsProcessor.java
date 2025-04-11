package org.enso.logging.service.telemetry;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ThreadPoolExecutor;
import org.enso.logging.service.telemetry.ApiMessage.Log;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Responsible for sending {@link LogMessage} to the endpoint asynchronously. */
public final class LogJobsProcessor {
  /**
   * We still want to limit the batch size to some reasonable number - sending too many logs in one
   * request could also be problematic.
   */
  private static final int MAX_BATCH_SIZE = 100;

  private static final int MAX_RETRIES = 5;
  private static final Logger LOGGER = LoggerFactory.getLogger(LogJobsProcessor.class);

  /**
   * The amount of time before the token expiration. Determines whether the token should be
   * proactively refreshed early, so that it does not expire during a request.
   */
  private static final Duration TOKEN_EARLY_REFRESH_PERIOD = Duration.ofMinutes(2);

  private final ThreadPoolExecutor backgroundThreadService;
  private final URI endpoint;
  private final LogJobsQueue logQueue = new LogJobsQueue();
  private final TokenRefresher tokenRefresher;
  private AuthenticationData authenticationData;
  private HttpClient httpClient;

  /**
   * Set to true once an error is encountered when sending a request. In such case, it is most
   * probably that no further requests will be successful. This flag is used to terminate the
   * background thread.
   */
  private boolean requestSendingFailure;

  public LogJobsProcessor(
      ThreadPoolExecutor executor,
      URI endpoint,
      AuthenticationData authenticationData,
      TokenRefresher tokenRefresher) {
    this.backgroundThreadService = Objects.requireNonNull(executor);
    this.endpoint = Objects.requireNonNull(endpoint);
    this.authenticationData = Objects.requireNonNull(authenticationData);
    this.tokenRefresher = tokenRefresher;
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
  public void enqueueMessage(LogJob logJob) {
    int queuedJobs = logQueue.enqueue(logJob);
    if (queuedJobs == 1 && backgroundThreadService.getQueue().isEmpty()) {
      // If we are the first message in the queue, we need to start the background thread.
      // It is possible that a job was already running, but adding a new one will not hurt - once
      // the queue is empty, the currently running job will finish and any additional jobs will also
      // terminate immediately.
      if (!requestSendingFailure) {
        backgroundThreadService.execute(this::logThreadEntryPoint);
      }
    }
  }

  /** Runs as long as there are any pending log messages queued and sends them in batches. */
  private void logThreadEntryPoint() {
    while (true) {
      List<LogJob> pendingMessages = logQueue.popEnqueuedJobs(MAX_BATCH_SIZE);
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
  private void sendBatch(List<LogJob> batch) throws RequestFailureException {
    assert !batch.isEmpty() : "The batch must not be empty.";

    if (accessTokenNeedsRefresh()) {
      LOGGER.debug("Refreshing access token");
      var refreshedAuthData = tokenRefresher.fetchNewAccessToken();
      if (refreshedAuthData != null) {
        authenticationData = refreshedAuthData;
        LOGGER.trace(
            "Token refreshed successfully. New expiration: {}", authenticationData.expireAt());
      } else {
        throw new RequestFailureException("Failed to refresh token", null);
      }
    }
    assert authenticationData != null;

    try {
      var request = buildRequest(batch);
      if (request == null) {
        LOGGER.warn("Failed to build request for log messages. Skipping {} messages", batch.size());
        throw new RequestFailureException("Cannot build request", null);
      }
      sendLogRequest(request, MAX_RETRIES);
      notifyJobsAboutSuccess(batch);
    } catch (RequestFailureException e) {
      notifyJobsAboutFailure(batch, e);
    }
  }

  private void notifyJobsAboutFailure(List<LogJob> logJobs, RequestFailureException exception) {
    LOGGER.warn("Failed to send {} log messages", logJobs.size(), exception);
    for (var job : logJobs) {
      if (job.completionNofitication() != null) {
        job.completionNofitication().completeExceptionally(exception);
      }
    }
  }

  private void notifyJobsAboutSuccess(List<LogJob> logJobs) {
    LOGGER.trace("Successfully sent {} log messages", logJobs.size());
    for (var logJob : logJobs) {
      if (logJob.completionNofitication() != null) {
        logJob.completionNofitication().complete(null);
      }
    }
  }

  private HttpRequest buildRequest(List<LogJob> logEvents) throws RequestFailureException {
    var payload = buildPayload(logEvents);
    if (payload != null) {
      return HttpRequest.newBuilder()
          .uri(endpoint)
          .header("Authorization", "Bearer " + authenticationData.accessToken())
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
  private String buildPayload(List<LogJob> logJobs) {
    var logs = new ArrayList<Log>();
    for (var logJob : logJobs) {
      var payloadForLogEvent = LogFormatter.transform(logJob.message());
      if (payloadForLogEvent != null) {
        logs.add(payloadForLogEvent);
      }
    }
    if (logs.size() != logJobs.size()) {
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

  private boolean accessTokenNeedsRefresh() {
    var now = Instant.now().atZone(ZoneId.of("UTC"));
    var inEarlyFuture = now.plus(TOKEN_EARLY_REFRESH_PERIOD);
    var expiration = authenticationData.expireAt();
    var res = inEarlyFuture.compareTo(expiration) > 0;
    LOGGER.trace(
        "Token needs refresh: {}. Current time (plus early refresh period): {}, expiration: {}",
        res,
        inEarlyFuture,
        expiration);
    return res;
  }

  private static final class RequestFailureException extends Exception {
    public RequestFailureException(String message, Throwable cause) {
      super(message, cause);
    }
  }
}
