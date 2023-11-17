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

  /** Substitutes the minimal parts within the string for the URI parse. */
  public static String encodeArg(String arg, boolean includeEquals) {
    var encoded = arg.replace("%", "%25")
        .replace("&", "%26")
        .replace(" ", "%20");
    if (includeEquals) {
      encoded = encoded.replace("=", "%3D");
    }
    return encoded;
  }

  private static String makeQueryAry(EnsoKeyValuePair pair, Function<EnsoKeyValuePair, String> resolver) {
    String resolvedKey = pair.key() != null && !pair.key().isBlank() ? encodeArg(pair.key(), true) + "=" : "";
    String resolvedValue = encodeArg(resolver.apply(pair), false);
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
      try {
        var baseURI = new URI(uri.getScheme(), uri.getAuthority(), uri.getPath(), null, null).toString();

        var baseQuery = uri.getQuery();
        baseQuery = baseQuery != null && !baseQuery.isBlank() ? "?" + baseQuery + "&" : "?";
        var query = baseQuery + queryArguments.stream().map(p -> makeQueryAry(p, EnsoSecretHelper::resolveValue)).collect(Collectors.joining("&"));

        var baseFragment = uri.getFragment();
        baseFragment = baseFragment != null && !baseFragment.isBlank() ? "#" + baseFragment : "";

        resolvedURI = URI.create(baseURI + query + baseFragment);
        renderedURI = resolvedURI;
        if (queryArguments.stream().anyMatch(p -> p instanceof EnsoKeySecretPair)) {
          if (!resolvedURI.getScheme().equals("https")) {
            // If used a secret then only allow HTTPS
            throw new IllegalArgumentException("Cannot use secrets in query string with non-HTTPS URI.");
          }

          var renderedQuery = baseQuery + queryArguments.stream().map(p -> makeQueryAry(p, EnsoSecretHelper::renderValue)).collect(Collectors.joining("&"));
          renderedURI = URI.create(baseURI + renderedQuery + baseFragment);
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
