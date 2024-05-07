package org.enso.base.enso_cloud.audit;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.ZonedDateTime;
import java.util.concurrent.ExecutorService;
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
class AuditLogAPI {
  private static final Logger logger = Logger.getLogger(AuditLogAPI.class.getName());
  public static AuditLogAPI INSTANCE = new AuditLogAPI();
  private final HttpClient httpClient =
      HttpClient.newBuilder().followRedirects(HttpClient.Redirect.ALWAYS).build();
  private final ExecutorService executorService;

  private AuditLogAPI() {
    // A thread pool that creates at most one thread, only when it is needed, and shuts it down
    // after 60 seconds of inactivity.
    // TODO we may want to set a maximum capacity to the queue and think how we deal with situations
    // when logs are added faster than they can be processed
    executorService =
        new ThreadPoolExecutor(0, 1, 60L, TimeUnit.SECONDS, new LinkedBlockingQueue<>());
  }

  public void logSync(LogMessage message) {
    var request = buildRequest(message);
    sendLogRequest(request, 5);
  }

  public Future<Void> logAsync(LogMessage message) {
    // We build the request on the main thread where the Enso Context is readily accessible - as we
    // need to access the `Authentication_Service`.
    var request = buildRequest(message);
    return executorService.submit(
        () -> {
          try {
            sendLogRequest(request, 5);
          } catch (RequestFailureException e) {
            throw e;
          } catch (Exception e) {
            logger.severe("Unexpected exception when sending a log message: " + e.getMessage());
            throw e;
          }
          return null;
        });
  }

  private HttpRequest buildRequest(LogMessage message) {
    var apiUri = CloudAPI.getAPIRootURI() + "logs";
    return HttpRequest.newBuilder()
        .uri(URI.create(apiUri))
        .header("Authorization", "Bearer " + AuthenticationProvider.getAccessToken())
        .POST(HttpRequest.BodyPublishers.ofString(message.payload(), StandardCharsets.UTF_8))
        .build();
  }

  private void sendLogRequest(HttpRequest request, int retryCount) throws RequestFailureException {
    try {
      try {
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
