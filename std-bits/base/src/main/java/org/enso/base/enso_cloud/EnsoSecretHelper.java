package org.enso.base.enso_cloud;

import io.github.resilience4j.retry.Retry;
import io.github.resilience4j.retry.RetryConfig;
import io.github.resilience4j.retry.RetryRegistry;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpRequest.Builder;
import java.net.http.HttpResponse;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.time.Duration;
import java.util.List;
import java.util.Properties;
import org.enso.base.net.URISchematic;
import org.enso.base.net.URIWithSecrets;
import org.graalvm.collections.Pair;

/** Makes HTTP requests with secrets in either header or query string. */
public final class EnsoSecretHelper extends SecretValueResolver {

  private static class CaughtCheckedException extends RuntimeException {
    private final Throwable origin;

    CaughtCheckedException(Throwable origin) {
      this.origin = origin;
    }

    boolean isIOException() {
      return origin instanceof IOException;
    }
  }

  private static final RetryConfig config =
      RetryConfig.custom()
          .maxAttempts(3)
          .waitDuration(Duration.ofMillis(100))
          .retryOnException(
              e -> {
                if (e instanceof CaughtCheckedException checked) return checked.isIOException();
                else return false;
              })
          .build();

  // Create a RetryRegistry with a custom global configuration
  private static final RetryRegistry registry = RetryRegistry.of(config);

  /** Gets a JDBC connection resolving EnsoKeyValuePair into the properties. */
  public static Connection getJDBCConnection(
      String url, List<Pair<String, HideableValue>> properties) throws SQLException {
    var javaProperties = new Properties();
    for (var pair : properties) {
      javaProperties.setProperty(pair.getLeft(), resolveValue(pair.getRight()));
    }

    return DriverManager.getConnection(url, javaProperties);
  }

  /**
   * Gets the actual URI with all secrets resolved, so that it can be used to create a request. This
   * value should never be returned to Enso.
   */
  private static URI resolveURI(URIWithSecrets uri) {
    try {
      List<Pair<String, String>> resolvedQueryParameters =
          uri.queryParameters().stream()
              .map(p -> Pair.create(p.getLeft(), resolveValue(p.getRight())))
              .toList();
      URISchematic resolvedSchematic = new URISchematic(uri.baseUri(), resolvedQueryParameters);
      return resolvedSchematic.build();
    } catch (URISyntaxException e) {
      // Here we don't display the message of the exception to avoid risking it may leak any
      // secrets.
      // This should never happen in practice.
      throw new IllegalStateException(
          "Unexpectedly unable to build a valid URI from the base URI: "
              + uri
              + ": "
              + e.getClass().getCanonicalName());
    }
  }

  /** Makes a request with secrets in the query string or headers. * */
  public static EnsoHttpResponse makeRequest(
      HttpClient client,
      Builder builder,
      URIWithSecrets uri,
      List<Pair<String, HideableValue>> headers,
      Boolean withRetries)
      throws IllegalArgumentException, IOException, InterruptedException {

    // Build a new URI with the query arguments.
    URI resolvedURI = resolveURI(uri);
    URI renderedURI = uri.render();

    boolean hasSecrets =
        uri.containsSecrets() || headers.stream().anyMatch(p -> p.getRight().containsSecrets());
    if (hasSecrets) {
      if (resolvedURI.getScheme() == null) {
        throw new IllegalArgumentException("The URI must have a scheme.");
      }

      if (!resolvedURI.getScheme().equalsIgnoreCase("https")) {
        throw new IllegalArgumentException(
            "Secrets are not allowed in HTTP connections, use HTTPS instead.");
      }
    }

    builder.uri(resolvedURI);

    // Resolve the header arguments.
    for (Pair<String, HideableValue> header : headers) {
      builder.header(header.getLeft(), resolveValue(header.getRight()));
    }

    // Build and Send the request.
    var httpRequest = builder.build();
    var bodyHandler = HttpResponse.BodyHandlers.ofInputStream();
    Retry retry = registry.retry("request");
    var decoratedSend =
        Retry.decorateFunction(
            retry,
            (HttpRequest request) -> {
              try {
                return client.send(request, bodyHandler);
              } catch (Throwable e) {
                throw new CaughtCheckedException(e);
              }
            });
    HttpResponse<InputStream> javaResponse;
    if (withRetries) {
      try {
        javaResponse = decoratedSend.apply(httpRequest);
      } catch (CaughtCheckedException e) {
        if (e.origin instanceof IOException ioe) {
          throw ioe;
        } else if (e.origin instanceof InterruptedException ie) {
          throw ie;
        } else {
          throw new IllegalStateException(e.origin);
        }
      }
    } else {
      javaResponse = client.send(httpRequest, bodyHandler);
    }
    // Extract parts of the response
    return new EnsoHttpResponse(
        renderedURI, javaResponse.headers(), javaResponse.body(), javaResponse.statusCode());
  }

  public static void deleteSecretFromCache(String secretId) {
    EnsoSecretReader.removeFromCache(secretId);
  }
}
