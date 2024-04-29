package org.enso.base.enso_cloud;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.util.logging.Logger;

public class AuditLog {
  private static final Logger logger = Logger.getLogger(AuditLog.class.getName());
  public static AuditLog INSTANCE = new AuditLog();
  private final HttpClient httpClient = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.ALWAYS).build();

  private AuditLog() {
  }

  public void logSync(LogMessage message) {
    sendLogRequest(message.payload, 3);
  }

  public void logAsync(LogMessage message) {
    // TODO
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
        HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
        if (response.statusCode() < 200 || response.statusCode() >= 300) {
          throw new RequestFailureException("Unexpected status code: " + response.statusCode() + " " + response.body(), null);
        }
      } catch (IOException | InterruptedException e) {
        // Promote a checked exception to a runtime exception to simplify the code.
        throw new RequestFailureException(e.getMessage(), e);
      }
    } catch (RequestFailureException e) {
      if (retryCount < 0) {
        logger.severe("Unable to send log message: " + e.getMessage());
        throw e;
      } else {
        logger.warning("Exception when sending a log message: " + e.getMessage() + ". Retrying...");
        sendLogRequest(payload, retryCount - 1);
      }
    }
  }

  public record LogMessage(String payload) {
  }

  public static class RequestFailureException extends RuntimeException {
    public RequestFailureException(String message, Throwable cause) {
      super(message, cause);
    }
  }
}
