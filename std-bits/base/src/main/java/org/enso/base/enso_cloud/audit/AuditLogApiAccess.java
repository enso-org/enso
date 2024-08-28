package org.enso.base.enso_cloud.audit;

import org.enso.base.enso_cloud.AuthenticationProvider;
import org.enso.base.enso_cloud.CloudAPI;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.logging.Logger;
import java.util.List;

/**
 * Gives access to the low-level log event API in the Cloud and manages asynchronously submitting the logs.
 */
class AuditLogApiAccess {
  private static final Logger logger = Logger.getLogger(AuditLogApiAccess.class.getName());

  /**
   * We still want to limit the batch size to some reasonable number - sending too many logs in one request could also
   * be problematic.
   */
  private static final int MAX_BATCH_SIZE = 100;

  private static final int MAX_RETRIES = 5;

  public static AuditLogApiAccess INSTANCE = new AuditLogApiAccess();

  private HttpClient httpClient;
  private LinkedBlockingDeque<LogJob> logQueue = new LinkedBlockingDeque<>();
  private RequestConfig requestConfig = null;
  private Thread logThread;

  private AuditLogApiAccess() {
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
    // If we ever make the queue size-constrained, this should become `put`.
    logQueue.add(job);
    if (logThread == null) {
      ensureLogThreadRunning();
    }
  }

  private synchronized void ensureLogThreadRunning() {
    if (logThread == null) {
      logThread = new Thread(this::logThreadEntryPoint);
      logThread.start();
    }
  }

  private void logThreadEntryPoint() {
    // TODO should the thread auto-shutdown after a while? then start logic needs to be smarter
    while (true) {
      // drainTo is non-blocking, so we first call `take` to wait for a first element to appear, and only then use
      // drain to get any other scheduled elements
      try {
        ArrayList<LogJob> pendingMessages = new ArrayList<>();
        pendingMessages.add(logQueue.take());
        logQueue.drainTo(pendingMessages, MAX_BATCH_SIZE - 1);

        try {
          var request = buildRequest(pendingMessages);
          sendLogRequest(request, MAX_RETRIES);
          notifyJobsAboutSuccess(pendingMessages);
        } catch (RequestFailureException e) {
          notifyJobsAboutFailure(pendingMessages, e);
        }
      } catch (InterruptedException e) {
        logger.warning("Log thread interrupted: " + e.getMessage());
        return;
      }
    }
  }

  private void notifyJobsAboutSuccess(List<LogJob> jobs) {
    for (var job : jobs) {
      if (job.completionNotification != null) {
        job.completionNotification.complete(null);
      }
    }
  }

  private void notifyJobsAboutFailure(List<LogJob> jobs, RequestFailureException e) {
    for (var job : jobs) {
      if (job.completionNotification != null) {
        job.completionNotification.completeExceptionally(e);
      }
    }
  }

  private HttpRequest buildRequest(List<LogJob> messages) {
    assert requestConfig != null : "The request configuration must be set before building a request.";
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
   * A record that represents a single log to be sent.
   * <p>
   * It may contain the `completionNotification` future that will be completed when the log is sent. If no-one is
   * listening for confirmation, that field will be `null`.
   */
  private record LogJob(LogMessage message, CompletableFuture<Void> completionNotification) {
  }

  /**
   * Contains information needed to build a request to the Cloud Logs API.
   * <p>
   * This information must be gathered on the main Enso thread, as only there we have access to the
   * {@link AuthenticationProvider}.
   */
  private record RequestConfig(URI apiUri, String accessToken) {
  }

  /**
   * Builds a request configuration based on runtime information. This method must be called from the main thread.
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
        throw new RequestFailureException("Failed to send log message: " + errorMessage, e);
      }
    } catch (RequestFailureException e) {
      if (retryCount < 0) {
        logger.severe("Failed to send log message after retrying: " + e.getMessage());
        failedLogCount++;
        throw e;
      } else {
        logger.warning("Exception when sending a log message: " + e.getMessage() + ". Retrying...");
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
   * A helper method to ensure that any changes to cloud environment configuration are reflected in the request
   * configuration.
   * <p>
   * This is only used in tests when the environment is being overridden. Normally, the environment does not change
   * during execution.
   */
  public static void refreshRequestConfig() {
    INSTANCE.requestConfig = INSTANCE.getRequestConfig();
  }
}
