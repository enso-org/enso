package org.enso.base.enso_cloud;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.http.HttpClient;
import java.net.http.HttpHeaders;
import java.net.http.HttpRequest.Builder;
import java.net.http.HttpResponse;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.List;
import java.util.Properties;
import java.util.function.Function;
import java.util.stream.Collectors;

/***
 * Makes HTTP requests with secrets in either header or query string.
 */
public class EnsoSecretHelper {
  private static String resolveValue(EnsoKeyValuePair pair) {
    return switch (pair) {
      case EnsoKeyStringPair stringPair -> stringPair.value();
      case EnsoKeySecretPair secretPair ->
          EnsoSecretReader.readSecret(secretPair.secretId());
      case null ->
          throw new IllegalArgumentException("EnsoKeyValuePair should not be NULL.");
    };
  }

  private static String renderValue(EnsoKeyValuePair pair) {
    return switch (pair) {
      case EnsoKeyStringPair stringPair -> stringPair.value();
      case EnsoKeySecretPair _ -> "__SECRET__";
      case null ->
          throw new IllegalArgumentException("EnsoKeyValuePair should not be NULL.");
    };
  }

  private static String makeQueryAry(EnsoKeyValuePair pair, Function<EnsoKeyValuePair, String> resolver) {
    String resolvedKey = pair.key() != null && !pair.key().isBlank() ? pair.key() + "=" : "";
    String resolvedValue = resolver.apply(pair);
    return resolvedKey + resolvedValue;
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
  public static EnsoHttpResponse makeRequest(HttpClient client, Builder builder, URI uri, List<EnsoKeyValuePair> queryArguments, List<EnsoKeyValuePair> headerArguments)
      throws IOException, InterruptedException {

    // Build a new URI with the query arguments.
    URI resolvedURI = uri;
    URI renderedURI = uri;
    if (queryArguments != null && !queryArguments.isEmpty()) {
      var baseQuery = uri.getQuery();
      var query = queryArguments.stream().map(p -> makeQueryAry(p, EnsoSecretHelper::resolveValue)).collect(Collectors.joining("&"));
      var newQuery = baseQuery != null && !baseQuery.isBlank() ? baseQuery + "&" + query : query;
      try {
        resolvedURI = new URI(uri.getScheme(), uri.getAuthority(), uri.getPath(), newQuery, uri.getFragment());
        renderedURI = resolvedURI;
        if (queryArguments.stream().anyMatch(p -> p instanceof EnsoKeySecretPair)) {
          if (!resolvedURI.getScheme().equals("https")) {
            // If used a secret then only allow HTTPS
            throw new IllegalArgumentException("Cannot use secrets in query string with non-HTTPS URI.");
          }

          var renderedQuery = queryArguments.stream().map(p -> makeQueryAry(p, EnsoSecretHelper::renderValue)).collect(Collectors.joining("&"));
          var newRenderedQuery = baseQuery != null && !baseQuery.isBlank() ? baseQuery + "&" + renderedQuery : renderedQuery;
          renderedURI = new URI(uri.getScheme(), uri.getAuthority(), uri.getPath(), newRenderedQuery, uri.getFragment());
        }
      } catch (URISyntaxException e) {
        throw new IllegalArgumentException("Unable to build a valid URI.");
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

  public record EnsoHttpResponse(URI uri, HttpHeaders headers, InputStream body, int statusCode) { }
}
