package org.enso.base.enso_cloud.audit;

import org.enso.base.enso_cloud.AuthenticationProvider;
import org.enso.base.enso_cloud.CloudAPI;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

/** Gives access to the low-level log event API in the Cloud and manages asynchronously submitting the logs. */
class AuditLogAPI {
  private static final Logger logger = Logger.getLogger(AuditLogAPI.class.getName());
  public static AuditLogAPI INSTANCE = new AuditLogAPI();
  private final HttpClient httpClient = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.ALWAYS).build();
  private final ExecutorService executorService;

  private AuditLogAPI() {
    // A thread pool that creates at most one thread, only when it is needed, and shuts it down after 60 seconds of inactivity.
    executorService = new ThreadPoolExecutor(0, 1, 60L, TimeUnit.SECONDS, new SynchronousQueue<>());
  }

  public void logSync(LogMessage message) {
    sendLogRequest(message.payload(), 5);
  }

  public Future<Void> logAsync(LogMessage message) {
    return executorService.submit(() -> {
      logSync(message);
      return null;
    });
  }

  private void sendLogRequest(String payload, int retryCount) throws RequestFailureException {
    var apiUri = CloudAPI.getAPIRootURI() + "logs";
    var request =
        HttpRequest.newBuilder()
            .uri(URI.create(apiUri))
            .header("Authorization", "Bearer " + AuthenticationProvider.getAccessToken())
            .POST(HttpRequest.BodyPublishers.ofString(payload, StandardCharsets.UTF_8))
            .build();

    try {
      try {
        System.out.println("Sending log message: " + payload);
        HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
        if (response.statusCode() < 200 || response.statusCode() >= 300) {
          throw new RequestFailureException("Unexpected status code: " + response.statusCode() + " " + response.body(), null);
        }
        System.out.println("Log message sent successfully: " + response.statusCode() + " " + response.body());
      } catch (IOException | InterruptedException e) {
        // Promote a checked exception to a runtime exception to simplify the code.
        throw new RequestFailureException(e.getMessage(), e);
      }
    } catch (RequestFailureException e) {
      System.out.println("Failed to send log message: " + e.getMessage());
      if (retryCount < 0) {
        logger.severe("Failed to send log message after retrying: " + e.getMessage());
        failedLogCount++;
        throw e;
      } else {
        logger.warning("Exception when sending a log message: " + e.getMessage() + ". Retrying...");
        sendLogRequest(payload, retryCount - 1);
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
