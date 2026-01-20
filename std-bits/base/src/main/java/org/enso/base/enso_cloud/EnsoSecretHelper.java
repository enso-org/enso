package org.enso.base.enso_cloud;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.http.HttpClient;
import java.net.http.HttpHeaders;
import java.net.http.HttpRequest.Builder;
import java.net.http.HttpResponse;
import java.security.PrivateKey;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.zip.GZIPInputStream;
import org.enso.base.cache.ReloadDetector;
import org.enso.base.cache.ResponseTooLargeException;
import org.enso.base.net.URISchematic;
import org.enso.base.net.URIWithSecrets;

/** Makes HTTP requests with secrets in either header or query string. */
public final class EnsoSecretHelper extends SecretValueResolver {
  private static EnsoHTTPResponseCache cache;

  /**
   * Gets a JDBC connection resolving EnsoKeyValuePair into the properties.
   *
   * @param properties properties in the form of {@code HideableValue.KeyValuePair}
   */
  public static Connection getJDBCConnection(
      String url, List<HideableValue.KeyValuePair> properties) throws SQLException {
    var javaProperties = new Properties();
    for (var pair : properties) {
      HideableValue value = pair.value();
      // Special handling for PrivateKey parameter.
      if (value instanceof HideableImpl.InterpretAsPrivateKey(HideableValue innerValue)) {
        String rawKey = resolveValue(innerValue);
        PrivateKey key = HideableImpl.InterpretAsPrivateKey.decodePrivateKey(rawKey);
        javaProperties.put(pair.key(), key);
      } else {
        javaProperties.setProperty(pair.key(), resolveValue(pair.value()));
      }
    }

    return DriverManager.getConnection(url, javaProperties);
  }

  /**
   * Gets the actual URI with all secrets resolved, so that it can be used to create a request. This
   * value should never be returned to Enso.
   */
  private static URI resolveURI(URIWithSecrets uri) {
    try {
      var resolvedQueryParameters =
          uri.queryParameters().stream()
              .map(p -> new AbstractMap.SimpleEntry<>(p.getKey(), resolveValue(p.getValue())))
              .toList();
      var resolvedSchematic = new URISchematic(uri.baseUri(), resolvedQueryParameters);
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
      Builder origBuilder,
      URIWithSecrets uri,
      List<Map.Entry<String, HideableValue>> headers,
      boolean useCache)
      throws IllegalArgumentException,
          IOException,
          InterruptedException,
          ResponseTooLargeException {
    // Clone incoming builder so we can't leak secrets through it
    var builder = origBuilder.copy();

    // Build a new URI with the query arguments.
    URI resolvedURI = resolveURI(uri);

    var resolvedHeaders =
        headers.stream()
            .map(
                pair -> {
                  return new AbstractMap.SimpleEntry<>(
                      pair.getKey(), resolveValue(pair.getValue()));
                })
            .toList();

    var requestMaker =
        new RequestMaker(client, builder, uri, resolvedURI, headers, resolvedHeaders);

    if (!useCache) {
      return requestMaker.makeRequest();
    } else {
      return getOrCreateCache().makeRequest(requestMaker);
    }
  }

  public static void deleteSecretFromCache(String secretId) {
    EnsoSecretReader.INSTANCE.removeFromCache(secretId);
  }

  private static class RequestMaker implements EnsoHTTPResponseCache.RequestMaker {
    private final HttpClient client;
    private final Builder builder;
    private final URIWithSecrets uri;
    private final URI resolvedURI;
    private final List<? extends Map.Entry<String, HideableValue>> headers;
    private final List<? extends Map.Entry<String, String>> resolvedHeaders;

    RequestMaker(
        HttpClient client,
        Builder builder,
        URIWithSecrets uri,
        URI resolvedURI,
        List<? extends Map.Entry<String, HideableValue>> headers,
        List<? extends Map.Entry<String, String>> resolvedHeaders) {
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
          uri.containsSecrets() || headers.stream().anyMatch(p -> p.getValue().containsSecrets());
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

      var resolvedHeadersWithDefaults = withDefaultHeaders(resolvedHeaders);
      for (var resolvedHeader : resolvedHeadersWithDefaults) {
        builder.header(resolvedHeader.getKey(), resolvedHeader.getValue());
      }

      // Build and Send the request.
      var httpRequest = builder.build();
      var bodyHandler = HttpResponse.BodyHandlers.ofInputStream();
      var javaResponse = client.send(httpRequest, bodyHandler);

      URI renderedURI = uri.render();

      var decodedBody = decodeContentEncoding(javaResponse.body(), javaResponse.headers());

      return new EnsoHttpResponse(
          renderedURI, javaResponse.headers(), decodedBody, javaResponse.statusCode());
    }

    /** Sorts the header by header name and value. */
    @Override
    public String hashKey() {
      // Include default headers in cache key to reflect actual request.
      var sortedHeaders =
          withDefaultHeaders(resolvedHeaders).stream().sorted(headerNameComparator).toList();
      List<String> keyStrings = new ArrayList<>(sortedHeaders.size() + 1);
      keyStrings.add(resolvedURI.toString());

      for (var resolvedHeader : sortedHeaders) {
        keyStrings.add(resolvedHeader.getKey());
        keyStrings.add(resolvedHeader.getValue());
      }

      return Integer.toHexString(Arrays.deepHashCode(keyStrings.toArray()));
    }

    @Override
    public EnsoHttpResponse reconstructResponseFromCachedStream(
        InputStream inputStream, EnsoHTTPResponseCache.Metadata metadata) {
      URI renderedURI = uri.render();

      return new EnsoHttpResponse(
          renderedURI, metadata.headers(), inputStream, metadata.statusCode());
    }
  }

  public static EnsoHTTPResponseCache getOrCreateCache() {
    if (cache == null) {
      cache = new EnsoHTTPResponseCache();
    }
    return cache;
  }

  /** Visible for testing */
  public static int getEnsoSecretReaderCacheSize() {
    return EnsoSecretReader.INSTANCE.getCacheSize();
  }

  /** Visible for testing */
  public static void simulateEnsoSecretReaderReload() {
    ReloadDetector.simulateReloadTestOnly(EnsoSecretReader.INSTANCE);
  }

  private static final Comparator<Map.Entry<String, String>> headerNameComparator =
      Comparator.comparing((Map.Entry<String, String> pair) -> pair.getKey())
          .thenComparing(Comparator.comparing(pair -> pair.getValue()));

  private static InputStream decodeContentEncoding(InputStream stream, HttpHeaders headers)
      throws IOException {
    String encoding = headers.firstValue("content-encoding").map(String::toLowerCase).orElse("");
    if ("gzip".equals(encoding)) {
      return new GZIPInputStream(stream);
    }
    return stream;
  }

  private static List<? extends Map.Entry<String, String>> withDefaultHeaders(
      List<? extends Map.Entry<String, String>> headers) {
    boolean hasAccept = false;
    boolean hasAcceptEncoding = false;
    boolean hasUserAgent = false;

    for (var h : headers) {
      var name = h.getKey();
      if ("accept".equalsIgnoreCase(name)) {
        hasAccept = true;
      } else if ("accept-encoding".equalsIgnoreCase(name)) {
        hasAcceptEncoding = true;
      } else if ("user-agent".equalsIgnoreCase(name)) {
        hasUserAgent = true;
      }
      if (hasAccept && hasAcceptEncoding && hasUserAgent) {
        return headers;
      }
    }

    var augmented = new ArrayList<Map.Entry<String, String>>(headers);
    if (!hasAccept) {
      augmented.add(new AbstractMap.SimpleEntry<>("Accept", "*/*"));
    }
    if (!hasAcceptEncoding) {
      augmented.add(new AbstractMap.SimpleEntry<>("Accept-Encoding", "gzip"));
    }
    if (!hasUserAgent) {
      augmented.add(new AbstractMap.SimpleEntry<>("User-Agent", "Enso-Client"));
    }
    return augmented;
  }
}
