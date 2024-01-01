package org.enso.base.enso_cloud;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.http.HttpClient;
import java.net.http.HttpRequest.Builder;
import java.net.http.HttpResponse;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.List;
import java.util.Properties;
import org.enso.base.net.URISchematic;
import org.enso.base.net.URIWithSecrets;
import org.graalvm.collections.Pair;

/** Makes HTTP requests with secrets in either header or query string. */
public class EnsoSecretHelper {
  /**
   * Gets the value of an HideableValue resolving secrets.
   *
   * @param value The value to resolve.
   * @return The pair's value. Should not be returned to Enso.
   */
  private static String resolveValue(HideableValue value) {
    return switch (value) {
      case HideableValue.PlainValue plainValue -> plainValue.value();
      case HideableValue.SecretValue secretValue -> EnsoSecretReader.readSecret(
          secretValue.secretId());
    };
  }

  /** Gets a JDBC connection resolving EnsoKeyValuePair into the properties. */
  public static Connection getJDBCConnection(String url, Pair<String, HideableValue>[] properties)
      throws SQLException {
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
      Pair<String, String> resolvedUserInfo =
          uri.userInfo() == null
              ? null
              : Pair.create(
                  resolveValue(uri.userInfo().username()), resolveValue(uri.userInfo().password()));
      URISchematic resolvedSchematic =
          new URISchematic(uri.baseUri(), resolvedQueryParameters, resolvedUserInfo);
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

  // ** Makes a request with secrets in the query string or headers. **//
  public static EnsoHttpResponse makeRequest(
      HttpClient client,
      Builder builder,
      URIWithSecrets uri,
      List<Pair<String, HideableValue>> headers)
      throws IOException, InterruptedException {

    // Build a new URI with the query arguments.
    URI resolvedURI = resolveURI(uri);
    URI renderedURI = uri.render();

    builder.uri(resolvedURI);

    // Resolve the header arguments.
    for (Pair<String, HideableValue> header : headers) {
      builder.header(header.getLeft(), resolveValue(header.getRight()));
    }

    // Build and Send the request.
    var httpRequest = builder.build();
    var bodyHandler = HttpResponse.BodyHandlers.ofInputStream();
    var javaResponse = client.send(httpRequest, bodyHandler);

    // Extract parts of the response
    return new EnsoHttpResponse(
        renderedURI, javaResponse.headers(), javaResponse.body(), javaResponse.statusCode());
  }
}
