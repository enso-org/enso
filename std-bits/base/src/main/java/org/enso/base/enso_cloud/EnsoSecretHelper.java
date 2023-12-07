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

/**
 * Makes HTTP requests with secrets in either header or query string.
 */
public class EnsoSecretHelper {
  /**
   * Gets the value of an EnsoKeyValuePair resolving secrets.
   * @param pair The pair to resolve.
   * @return The pair's value. Should not be returned to Enso.
   */
  private static String resolveValue(EnsoKeyValuePair pair) {
    return switch (pair) {
      case EnsoKeyStringPair stringPair -> stringPair.value();
      case EnsoKeySecretPair secretPair ->
          EnsoSecretReader.readSecret(secretPair.secretId());
      case null ->
          throw new IllegalArgumentException("EnsoKeyValuePair should not be NULL.");
    };
  }

  /**
   * Converts an EnsoKeyValuePair into a string for display purposes. Does not include secrets.
   * @param pair The pair to render.
   * @return The rendered string.
   */
  private static String renderValue(EnsoKeyValuePair pair) {
    return switch (pair) {
      case EnsoKeyStringPair stringPair -> stringPair.value();
      case EnsoKeySecretPair _ -> "__SECRET__";
      case null ->
          throw new IllegalArgumentException("EnsoKeyValuePair should not be NULL.");
    };
  }

  /**
   * Substitutes the minimal parts within the string for the URI parse.
   * */
  public static String encodeArg(String arg, boolean includeEquals) {
    var encoded = arg.replace("%", "%25")
        .replace("&", "%26")
        .replace(" ", "%20");
    if (includeEquals) {
      encoded = encoded.replace("=", "%3D");
    }
    return encoded;
  }

    /**
     * Replaces the query string in a URI.
     * */
  public static URI replaceQuery(URI uri, String newQuery) throws URISyntaxException {
    var baseURI = new URI(uri.getScheme(), uri.getAuthority(), uri.getPath(), null, null).toString();

    var baseFragment = uri.getFragment();
    baseFragment = baseFragment != null && !baseFragment.isBlank() ? "#" + baseFragment : "";

    return URI.create(baseURI + newQuery + baseFragment);
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
        var baseQuery = uri.getQuery();
        baseQuery = baseQuery != null && !baseQuery.isBlank() ? "?" + baseQuery + "&" : "?";
        var query = baseQuery + queryArguments.stream().map(p -> makeQueryAry(p, EnsoSecretHelper::resolveValue)).collect(Collectors.joining("&"));

        resolvedURI = replaceQuery(uri, query);
        renderedURI = resolvedURI;
        if (queryArguments.stream().anyMatch(p -> p instanceof EnsoKeySecretPair)) {
          if (!resolvedURI.getScheme().equals("https")) {
            // If used a secret then only allow HTTPS
            throw new IllegalArgumentException("Cannot use secrets in query string with non-HTTPS URI.");
          }

          var renderedQuery = baseQuery + queryArguments.stream().map(p -> makeQueryAry(p, EnsoSecretHelper::renderValue)).collect(Collectors.joining("&"));
          renderedURI = replaceQuery(uri, renderedQuery);
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
    return new EnsoHttpResponse(renderedURI, javaResponse.headers().map().keySet().stream().toList(), javaResponse.headers(), javaResponse.body(), javaResponse.statusCode());
  }

  /**
   * A subset of the HttpResponse to avoid leaking the decrypted Enso secrets.
   */
  public record EnsoHttpResponse(URI uri, List<String> headerNames, HttpHeaders headers, InputStream body, int statusCode) { }
}
