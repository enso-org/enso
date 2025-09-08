package org.enso.logging.service.common;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneId;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ThreadPoolExecutor;
import org.slf4j.Logger;

/** Responsible for sending {@link LogMessage} to the endpoint asynchronously. */
public abstract class LogJobsProcessor {
  /**
   * We still want to limit the batch size to some reasonable number - sending too many logs in one
   * request could also be problematic.
   */
  private static final int MAX_BATCH_SIZE = 100;

  private static final int MAX_RETRIES = 5;
  protected final Logger logger;

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
  private final boolean logConnectionFailures;

  public LogJobsProcessor(
      ThreadPoolExecutor executor,
      URI endpoint,
      AuthenticationData authenticationData,
      TokenRefresher tokenRefresher,
      boolean logConnectionFailures,
      Logger logger) {
    this.backgroundThreadService = Objects.requireNonNull(executor);
    this.endpoint = Objects.requireNonNull(endpoint);
    this.authenticationData = Objects.requireNonNull(authenticationData);
    this.tokenRefresher = Objects.requireNonNull(tokenRefresher);
    this.logConnectionFailures = logConnectionFailures;
    this.logger = logger;
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
      backgroundThreadService.execute(this::logThreadEntryPoint);
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

      if (accessTokenNeedsRefresh()) {
        logger.trace("Refreshing access token");
        var refreshedAuthData = fetchNewAccessToken();
        if (refreshedAuthData != null) {
          authenticationData = refreshedAuthData;
          logger.trace(
              "Token refreshed successfully. New expiration: {}", authenticationData.expireAt());
        } else {
          notifyJobsAboutFailure(
              pendingMessages, new RequestFailureException("Failed to refresh token", null));
          return;
        }
      }
      assert authenticationData != null;
      sendBatch(pendingMessages);
    }
  }

  /**
   * Sends a batch of log messages.
   *
   * <p>The batch must not be empty and all messages must share the same request config.
   */
  private void sendBatch(List<LogJob> batch) {
    assert !batch.isEmpty() : "The batch must not be empty.";
    assert authenticationData != null;
    try {
      var request = buildRequest(batch);
      if (request == null) {
        logger.warn("Failed to build request for log messages. Skipping {} messages", batch.size());
        throw new RequestFailureException("Cannot build request", null);
      }
      sendLogRequest(request, MAX_RETRIES);
      notifyJobsAboutSuccess(batch);
    } catch (RequestFailureException e) {
      notifyJobsAboutFailure(batch, e);
    }
  }

  private void notifyJobsAboutFailure(List<LogJob> logJobs, RequestFailureException exception) {
    if (logConnectionFailures) {
      logger.warn("Failed to send {} log messages", logJobs.size(), exception);
    }
    for (var job : logJobs) {
      if (job.completionNofitication() != null) {
        job.completionNofitication().completeExceptionally(exception);
      }
    }
  }

  private void notifyJobsAboutSuccess(List<LogJob> logJobs) {
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

  protected abstract String buildPayload(List<LogJob> logJobs);

  protected void sendLogRequest(HttpRequest request, int retryCount)
      throws RequestFailureException {
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
        if (logConnectionFailures) {
          logger.debug("Failed to send log messages after retrying", e);
        }
        throw e;
      } else {
        if (logConnectionFailures) {
          logger.trace("Exception when sending log messages. Retrying...", e);
        }
        sendLogRequest(request, retryCount - 1);
      }
    }
  }

  private AuthenticationData fetchNewAccessToken() {
    for (int i = 0; i < MAX_RETRIES; i++) {
      var refreshedAuthData = tokenRefresher.fetchNewAccessToken();
      if (refreshedAuthData != null) {
        return refreshedAuthData;
      } else {
        logger.warn("Failed to refresh token. Retrying... (attempt {}/{})", i + 1, MAX_RETRIES);
      }
    }
    return null;
  }

  private boolean accessTokenNeedsRefresh() {
    var now = Instant.now().atZone(ZoneId.of("UTC"));
    var inEarlyFuture = now.plus(TOKEN_EARLY_REFRESH_PERIOD);
    var expiration = authenticationData.expireAt();
    var res = inEarlyFuture.compareTo(expiration) > 0;
    if (res) {
      logger.trace(
          "Token needs refresh: {}. Current time (plus early refresh period): {}, expiration: {}",
          res,
          inEarlyFuture,
          expiration);
    }
    return res;
  }

  private static final class RequestFailureException extends Exception {
    public RequestFailureException(String message, Throwable cause) {
      super(message, cause);
    }
  }
}
