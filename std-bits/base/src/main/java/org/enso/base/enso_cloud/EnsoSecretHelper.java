package org.enso.base.enso_cloud;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest.Builder;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.List;
import java.util.Properties;
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

  private static String makeQueryAry(EnsoKeyValuePair pair) {
    String resolvedKey = pair.key() != null && !pair.key().isBlank() ? pair.key() + "=" : "";
    String resolvedValue = resolveValue(pair);
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
  public static HttpResponse<InputStream> makeRequest(HttpClient client, Builder builder, URI uri, List<EnsoKeyValuePair> queryArguments, List<EnsoKeyValuePair> headerArguments)
      throws IOException, InterruptedException {
    // Build a new URI with the query arguments.
    if (queryArguments != null && !queryArguments.isEmpty()) {
      var baseQuery = uri.getQuery();
      var query = queryArguments.stream().map(EnsoSecretHelper::makeQueryAry).collect(Collectors.joining("&"));
      var newQuery = baseQuery != null && !baseQuery.isBlank() ? baseQuery + "&" + query : query;
      try {
        uri = new URI(uri.getScheme(), uri.getAuthority(), uri.getPath(), newQuery, uri.getFragment());
      } catch (URISyntaxException e) {
        throw new IllegalArgumentException("Unable to build a valid URI.");
      }
    }
    builder.uri(uri);

    // Resolve the header arguments.
    if (headerArguments != null && !headerArguments.isEmpty()) {
      for (EnsoKeyValuePair header : headerArguments) {
        builder.header(header.key(), resolveValue(header));
      }
    }

    // If used a secret then only allow HTTPS

    // Build and Send the request.
    var httpRequest = builder.build();
    var bodyHandler = HttpResponse.BodyHandlers.ofInputStream();
    return client.send(httpRequest, bodyHandler);
  }
}
