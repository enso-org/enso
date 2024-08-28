package org.enso.base.enso_cloud.audit;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
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
  private RequestConfig requestConfig = null;
  private final ThreadPoolExecutor backgroundThreadService;

  private AuditLogApiAccess() {
    // We set-up a thread 'pool' that will contain at most one thread.
    // If the thread is idle for 60 seconds, it will be shut down.
    backgroundThreadService =
        new ThreadPoolExecutor(0, 1, 60L, TimeUnit.SECONDS, new LinkedBlockingQueue<>());
  }

  public Future<Void> logWithConfirmation(LogMessage message) {
    CompletableFuture<Void> completionNotification = new CompletableFuture<>();
    enqueueJob(new LogJob(message, completionNotification));
    return completionNotification;
  }

  public void logWithoutConfirmation(LogMessage message) {
    enqueueJob(new LogJob(message, null));
  }

  private void enqueueJob(LogJob job) {
    ensureConfigSaved();
    int queuedJobs = logQueue.enqueue(job);
    if (queuedJobs == 1) {
      // If we are the first job in the queue, we need to start the background thread.
      // It is possible that a job was already running, but adding a new one will not hurt - once
      // the queue is empty, the currently running job will finish and any additional jobs will also
      // terminate immediately.
      backgroundThreadService.execute(this::logThreadEntryPoint);
    }

    /*
     * Liveness is guaranteed, because the queue size always increments exactly by 1, so we `enqueue` returns 1 if and only if the queue was empty beforehand.
     * If the queue was empty before adding a job, we always schedule a `logThreadEntryPoint` to run.
     * Any running `logThreadEntryPoint` will not finish until the queue is empty.
     * So after every append, either the thread is surely running or it is started.
     */
  }

  /** Runs as long as there are any pending log messages queued and sends them in batches. */
  private void logThreadEntryPoint() {
    while (true) {
      List<LogJob> pendingMessages = logQueue.popEnqueuedJobs(MAX_BATCH_SIZE);
      if (pendingMessages.isEmpty()) {
        // If there are no more pending messages, we can stop the thread for now. It will be
        // re-launched if needed.
        return;
      }

      try {
        var request = buildRequest(pendingMessages);
        sendLogRequest(request, MAX_RETRIES);
        notifyJobsAboutSuccess(pendingMessages);
      } catch (RequestFailureException e) {
        notifyJobsAboutFailure(pendingMessages, e);
      }
    }
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

  private HttpRequest buildRequest(List<LogJob> messages) {
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
    payload.append("[");
    for (var message : messages) {
      payload.append(message.message().payload()).append(",");
    }

    // replace the last comma with a closing bracket
    payload.replace(payload.length() - 1, payload.length(), "]");
    return payload.toString();
  }

  /**
   * Contains information needed to build a request to the Cloud Logs API.
   *
   * <p>This information must be gathered on the main Enso thread, as only there we have access to
   * the {@link AuthenticationProvider}.
   */
  private record RequestConfig(URI apiUri, String accessToken) {}

  /**
   * Builds a request configuration based on runtime information. This method must be called from
   * the main thread.
   */
  private RequestConfig getRequestConfig() {
    var uri = URI.create(CloudAPI.getAPIRootURI() + "logs");
    return new RequestConfig(uri, AuthenticationProvider.getAccessToken());
  }

  private void ensureConfigSaved() {
    if (requestConfig == null) {
      requestConfig = getRequestConfig();
    }
  }

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
        failedLogCount++;
        throw e;
      } else {
        logger.warning("Exception when sending log messages: " + e.getMessage() + ". Retrying...");
        sendLogRequest(request, retryCount - 1);
      }
    }
  }

  public interface LogMessage {
    String payload();
  }

  public static class RequestFailureException extends RuntimeException {
    public RequestFailureException(String message, Throwable cause) {
      super(message, cause);
    }
  }

  private int failedLogCount = 0;

  public int getFailedLogCount() {
    return failedLogCount;
  }

  /**
   * A helper method to ensure that any changes to cloud environment configuration are reflected in
   * the request configuration.
   *
   * <p>This is only used in tests when the environment is being overridden. Normally, the
   * environment does not change during execution.
   */
  public static void refreshRequestConfig() {
    INSTANCE.requestConfig = INSTANCE.getRequestConfig();
  }
}
