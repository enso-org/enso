package org.enso.base.enso_cloud;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.http.HttpClient;
import java.net.http.HttpRequest.Builder;
import java.net.http.HttpResponse;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Properties;
import org.enso.base.cache.ResponseTooLargeException;
import org.enso.base.net.URISchematic;
import org.enso.base.net.URIWithSecrets;
import org.graalvm.collections.Pair;

/** Makes HTTP requests with secrets in either header or query string. */
public final class EnsoSecretHelper extends SecretValueResolver {

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
      boolean useCache)
      throws IllegalArgumentException,
          IOException,
          InterruptedException,
          ResponseTooLargeException {

    // Build a new URI with the query arguments.
    URI resolvedURI = resolveURI(uri);

    List<Pair<String, String>> resolvedHeaders =
        headers.stream()
            .map(
                pair -> {
                  return Pair.create(pair.getLeft(), resolveValue(pair.getRight()));
                })
            .toList();

    var requestMaker =
        new RequestMaker(client, builder, uri, resolvedURI, headers, resolvedHeaders);

    if (!useCache) {
      return requestMaker.makeRequest();
    } else {
      return EnsoHTTPResponseCache.makeRequest(requestMaker);
    }
  }

  public static void deleteSecretFromCache(String secretId) {
    EnsoSecretReader.removeFromCache(secretId);
  }

  private static class RequestMaker implements EnsoHTTPResponseCache.RequestMaker {
    private final HttpClient client;
    private final Builder builder;
    private final URIWithSecrets uri;
    private final URI resolvedURI;
    private final List<Pair<String, HideableValue>> headers;
    private final List<Pair<String, String>> resolvedHeaders;

    RequestMaker(
        HttpClient client,
        Builder builder,
        URIWithSecrets uri,
        URI resolvedURI,
        List<Pair<String, HideableValue>> headers,
        List<Pair<String, String>> resolvedHeaders) {
      this.client = client;
      this.builder = builder;
      this.uri = uri;
      this.resolvedURI = resolvedURI;
      this.headers = headers;
      this.resolvedHeaders = resolvedHeaders;
    }

    @Override
    public EnsoHttpResponse makeRequest() throws IOException, InterruptedException {
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

      for (Pair<String, String> resolvedHeader : resolvedHeaders) {
        builder.header(resolvedHeader.getLeft(), resolvedHeader.getRight());
      }

      // Build and Send the request.
      var httpRequest = builder.build();
      var bodyHandler = HttpResponse.BodyHandlers.ofInputStream();
      var javaResponse = client.send(httpRequest, bodyHandler);

      URI renderedURI = uri.render();

      return new EnsoHttpResponse(
          renderedURI, javaResponse.headers(), javaResponse.body(), javaResponse.statusCode());
    }

    /** Sorts the header by header name and value. */
    @Override
    public String hashKey() {
      var sortedHeaders = resolvedHeaders.stream().sorted(headerNameComparator).toList();
      List<String> keyStrings = new ArrayList<>(sortedHeaders.size() + 1);
      keyStrings.add(resolvedURI.toString());

      for (Pair<String, String> resolvedHeader : sortedHeaders) {
        keyStrings.add(resolvedHeader.getLeft());
        keyStrings.add(resolvedHeader.getRight());
      }

      return Integer.toString(Arrays.deepHashCode(keyStrings.toArray()));
    }

    @Override
    public EnsoHttpResponse reconstructResponseFromCachedStream(
        InputStream inputStream, EnsoHTTPResponseCache.Metadata metadata) {
      URI renderedURI = uri.render();

      return new EnsoHttpResponse(
          renderedURI, metadata.headers(), inputStream, metadata.statusCode());
    }
  }

  private static final Comparator<Pair<String, String>> headerNameComparator =
      Comparator.comparing((Pair<String, String> pair) -> pair.getLeft())
          .thenComparing(Comparator.comparing(pair -> pair.getRight()));
}
