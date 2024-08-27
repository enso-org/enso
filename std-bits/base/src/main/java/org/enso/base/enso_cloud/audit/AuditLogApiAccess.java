package org.enso.base.enso_cloud.audit;

import org.enso.base.enso_cloud.AuthenticationProvider;
import org.enso.base.enso_cloud.CloudAPI;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

/**
 * Gives access to the low-level log event API in the Cloud and manages asynchronously submitting
 * the logs.
 */
class AuditLogApiAccess {
  private static final Logger logger = Logger.getLogger(AuditLogApiAccess.class.getName());

  /**
   * We still want to limit the batch size to some reasonable number - sending too many logs in one request could also be problematic.
   */
  private static final int MAX_BATCH_SIZE = 100;

  private static final int MAX_RETRIES = 5;

  public static AuditLogApiAccess INSTANCE = new AuditLogApiAccess();

  private HttpClient httpClient;
  private LinkedBlockingDeque<LogJob> logQueue = new LinkedBlockingDeque<>();

  private AuditLogApiAccess() {

  }

  public Future<Void> logSync(LogMessage message) {
    CompletableFuture<Void> completionNotification = new CompletableFuture<>();
    enqueueJob(new LogJob(message, completionNotification));
    return completionNotification;
  }

  public void logAsync(LogMessage message) {
    enqueueJob(new LogJob(message, null));
  }

  private void enqueueJob(LogJob job) {
    // TODO
  }

  private HttpRequest buildRequest(LogMessage message) {
    String apiUri = CloudAPI.getAPIRootURI() + "logs";
    return HttpRequest.newBuilder()
        .uri(URI.create(apiUri))
        .header("Authorization", "Bearer " + AuthenticationProvider.getAccessToken())
        .POST(HttpRequest.BodyPublishers.ofString(message.payload(), StandardCharsets.UTF_8))
        .build();
  }

  /**
   * A record that represents a single log to be sent.
   * <p>
   * It may contain the `completionNotification` future that will be completed when the log is sent.
   * If no-one is listening for confirmation, that field will be `null`.
   */
  private record LogJob(LogMessage message, CompletableFuture<Void> completionNotification) {
  }

  /**
   * Contains information needed to build a request to the Cloud Logs API.
   * <p>
   * This information must be gathered on the main Enso thread, as only there we have access to the {@link AuthenticationProvider}.
   */
  private record RequestConfig(URI apiUri, String accessToken) {
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
}
