package org.enso.base.enso_cloud.audit;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import org.enso.base.enso_cloud.AuthenticationProvider;
import org.enso.base.enso_cloud.CloudAPI;

/**
 * Gives access to the low-level log event API in the Cloud and manages asynchronously submitting
 * the logs.
 */
class AuditLogApiAccess {
  private static final Logger logger = Logger.getLogger(AuditLogApiAccess.class.getName());

  /**
   * We still want to limit the batch size to some reasonable number - sending too many logs in one
   * request could also be problematic.
   */
  private static final int MAX_BATCH_SIZE = 100;

  private static final int MAX_RETRIES = 5;

  public static AuditLogApiAccess INSTANCE = new AuditLogApiAccess();

  private HttpClient httpClient;
  private final LogJobsQueue logQueue = new LogJobsQueue();
  private final ThreadPoolExecutor backgroundThreadService;

  private AuditLogApiAccess() {
    // We set-up a thread 'pool' that will contain at most one thread.
    // If the thread is idle for 60 seconds, it will be shut down.
    backgroundThreadService =
        new ThreadPoolExecutor(0, 1, 60L, TimeUnit.SECONDS, new LinkedBlockingQueue<>());
  }

  public Future<Void> logWithConfirmation(LogMessage message) {
    var currentRequestConfig = getRequestConfig();
    CompletableFuture<Void> completionNotification = new CompletableFuture<>();
    enqueueJob(new LogJob(message, completionNotification, currentRequestConfig));
    return completionNotification;
  }

  public void logWithoutConfirmation(LogMessage message) {
    var currentRequestConfig = getRequestConfig();
    enqueueJob(new LogJob(message, null, currentRequestConfig));
  }

  private void enqueueJob(LogJob job) {
    int queuedJobs = logQueue.enqueue(job);
    if (queuedJobs == 1 && backgroundThreadService.getQueue().isEmpty()) {
      // If we are the first message in the queue, we need to start the background thread.
      // It is possible that a job was already running, but adding a new one will not hurt - once
      // the queue is empty, the currently running job will finish and any additional jobs will also
      // terminate immediately.
      backgroundThreadService.execute(this::logThreadEntryPoint);
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
      List<LogJob> pendingMessages = logQueue.popEnqueuedJobs(MAX_BATCH_SIZE);
      if (pendingMessages.isEmpty()) {
        // If there are no more pending messages, we can stop the thread for now.
        // If during this teardown a new message is added, it will see no elements on `logQueue` and
        // thus,
        // `logQueue.enqueue` will return 1, thus ensuring that at least one new job is scheduled.
        return;
      }

      var batchesByConfig = splitMessagesByConfig(pendingMessages);
      for (var batch : batchesByConfig) {
        sendBatch(batch);
      }
    }
  }

  /**
   * Sends a batch of log messages.
   *
   * <p>The batch must not be empty and all messages must share the same request config.
   */
  private void sendBatch(List<LogJob> batch) {
    assert !batch.isEmpty() : "The batch must not be empty.";
    // We use the request config from the first message - all messages in the batch should have the
    // same request config.
    var requestConfig = batch.get(0).requestConfig();
    assert requestConfig != null
        : "The request configuration must be set before building a request.";
    assert batch.stream().allMatch(job -> job.requestConfig().equals(requestConfig))
        : "All messages in a batch must have the same request configuration.";

    try {
      var request = buildRequest(requestConfig, batch);
      sendLogRequest(request, MAX_RETRIES);
      notifyJobsAboutSuccess(batch);
    } catch (RequestFailureException e) {
      notifyJobsAboutFailure(batch, e);
    }
  }

  /**
   * Only during testing, it is possible to encounter pending messages with different request
   * configs (when the config changes between tests). To send each message where it is intended, we
   * split up the batch by the config.
   */
  Collection<List<LogJob>> splitMessagesByConfig(List<LogJob> messages) {
    HashMap<RequestConfig, List<LogJob>> hashMap = new HashMap<>();
    for (var message : messages) {
      var list = hashMap.computeIfAbsent(message.requestConfig(), k -> new ArrayList<>());
      list.add(message);
    }

    return hashMap.values();
  }

  private void notifyJobsAboutSuccess(List<LogJob> jobs) {
    for (var job : jobs) {
      if (job.completionNotification() != null) {
        job.completionNotification().complete(null);
      }
    }
  }

  private void notifyJobsAboutFailure(List<LogJob> jobs, RequestFailureException e) {
    for (var job : jobs) {
      if (job.completionNotification() != null) {
        job.completionNotification().completeExceptionally(e);
      }
    }
  }

  private HttpRequest buildRequest(RequestConfig requestConfig, List<LogJob> messages) {
    assert requestConfig != null
        : "The request configuration must be set before building a request.";
    var payload = buildPayload(messages);
    return HttpRequest.newBuilder()
        .uri(requestConfig.apiUri)
        .header("Authorization", "Bearer " + requestConfig.accessToken)
        .POST(HttpRequest.BodyPublishers.ofString(payload, StandardCharsets.UTF_8))
        .build();
  }

  private String buildPayload(List<LogJob> messages) {
    var payload = new StringBuilder();
    payload.append("{\"logs\": [");
    for (var message : messages) {
      payload.append(message.message().payload()).append(",");
    }

    // Remove the trailing comma.
    payload.deleteCharAt(payload.length() - 1);
    payload.append("]}");
    return payload.toString();
  }

  private RequestConfig cachedRequestConfig = null;

  /**
   * Builds a request configuration based on runtime information.
   *
   * <p>This method must be called from the main thread.
   *
   * <p>The same instance is returned every time after the first call, unless the caches were
   * flushed (which is mostly used in tests).
   */
  private RequestConfig getRequestConfig() {
    if (cachedRequestConfig != null) {
      return cachedRequestConfig;
    }

    var uri = URI.create(CloudAPI.getAPIRootURI() + "logs");
    var config = new RequestConfig(uri, AuthenticationProvider.getAccessToken());
    cachedRequestConfig = config;
    return config;
  }

  /**
   * Contains information needed to build a request to the Cloud Logs API.
   *
   * <p>This information must be gathered on the main Enso thread, as only there we have access to
   * the {@link AuthenticationProvider}.
   *
   * <p>We associate an instance with every message to be sent. When sending multiple messages in a
   * batch, we will use the config from one of them. This should not matter as in normal operations
   * the configs will be the same, they only change during testing. Tests should this into account,
   * by sending the last message in synchronous mode.
   */
  private record RequestConfig(URI apiUri, String accessToken) {}

  private void sendLogRequest(HttpRequest request, int retryCount) throws RequestFailureException {
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
        logger.severe("Failed to send log messages after retrying: " + e.getMessage());
        throw e;
      } else {
        logger.warning("Exception when sending log messages: " + e.getMessage() + ". Retrying...");
        sendLogRequest(request, retryCount - 1);
      }
    }
  }

  interface LogMessage {
    String payload();
  }

  static class RequestFailureException extends RuntimeException {
    public RequestFailureException(String message, Throwable cause) {
      super(message, cause);
    }
  }

  /**
   * A record that represents a single log to be sent.
   *
   * <p>It may contain the `completionNotification` future that will be completed when the log is
   * sent. If no-one is listening for confirmation, that field will be `null`.
   */
  record LogJob(
      LogMessage message,
      CompletableFuture<Void> completionNotification,
      RequestConfig requestConfig) {}

  void resetCache() {
    cachedRequestConfig = null;
  }
}
