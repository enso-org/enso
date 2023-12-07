package org.enso.base.enso_cloud;

import org.enso.base.net.URIHelpers;

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

/**
 * Makes HTTP requests with secrets in either header or query string.
 */
public class EnsoSecretHelper {
  /**
   * Gets the value of an EnsoKeyValuePair resolving secrets.
   *
   * @param pair The pair to resolve.
   * @return The pair's value. Should not be returned to Enso.
   */
  private static String resolveValue(EnsoKeyValuePair pair) {
    return switch (pair) {
      case EnsoKeyStringPair stringPair -> stringPair.value();
      case EnsoKeySecretPair secretPair -> EnsoSecretReader.readSecret(secretPair.secretId());
      case null -> throw new IllegalArgumentException("EnsoKeyValuePair should not be NULL.");
    };
  }

  /**
   * Converts an EnsoKeyValuePair into a string for display purposes. Does not include secrets.
   *
   * @param pair The pair to render.
   * @return The rendered string.
   */
  private static String renderValue(EnsoKeyValuePair pair) {
    return switch (pair) {
      case EnsoKeyStringPair stringPair -> stringPair.value();
      case EnsoKeySecretPair _ -> "__SECRET__";
      case null -> throw new IllegalArgumentException("EnsoKeyValuePair should not be NULL.");
    };
  }

  //** Gets a JDBC connection resolving EnsoKeyValuePair into the properties. **//
  public static Connection getJDBCConnection(String url, EnsoKeyValuePair[] properties)
      throws SQLException {
    var javaProperties = new Properties();
    for (EnsoKeyValuePair pair : properties) {
      javaProperties.setProperty(pair.key(), resolveValue(pair));
    }

    return DriverManager.getConnection(url, javaProperties);
  }

  //** Makes a request with secrets in the query string or headers. **//
  public static EnsoHttpResponse makeRequest(HttpClient client, Builder builder, URI uri,
                                             List<EnsoKeyValuePair> queryArguments,
                                             List<EnsoKeyValuePair> headerArguments)
      throws IOException, InterruptedException {

    // Build a new URI with the query arguments.
    URI resolvedURI = uri;
    URI renderedURI = uri;
    if (queryArguments != null && !queryArguments.isEmpty()) {
      boolean hasSecrets = queryArguments.stream().anyMatch(p -> p instanceof EnsoKeySecretPair);
      if (hasSecrets && !uri.getScheme().equals("https")) {
        // If used a secret then only allow HTTPS
        throw new IllegalArgumentException("Cannot use secrets in query string with non-HTTPS URI, but the scheme " +
            "was: " + uri.getScheme() + ".");
      }

      try {
        List<URIHelpers.NameValuePair> resolvedArguments = queryArguments.stream()
            .map(p -> new URIHelpers.NameValuePair(p.key(), resolveValue(p)))
            .toList();
        List<URIHelpers.NameValuePair> renderedArguments = queryArguments.stream()
            .map(p -> new URIHelpers.NameValuePair(p.key(), renderValue(p)))
            .toList();

        resolvedURI = URIHelpers.addQueryParameters(uri, resolvedArguments);
        renderedURI = URIHelpers.addQueryParameters(uri, renderedArguments);
      } catch (URISyntaxException e) {
        throw new IllegalStateException(
            "Unexpectedly unable to build a valid URI from the base URI: " + uri + " and query arguments: " + queryArguments + "."
        );
      }
    }
    builder.uri(resolvedURI);

    // Resolve the header arguments.
    if (headerArguments != null && !headerArguments.isEmpty()) {
      for (EnsoKeyValuePair header : headerArguments) {
        builder.header(header.key(), resolveValue(header));
      }
    }

    // Build and Send the request.
    var httpRequest = builder.build();
    var bodyHandler = HttpResponse.BodyHandlers.ofInputStream();
    var javaResponse = client.send(httpRequest, bodyHandler);

    // Extract parts of the response
    return new EnsoHttpResponse(renderedURI, javaResponse.headers(), javaResponse.body(), javaResponse.statusCode());
  }
}
