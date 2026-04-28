package org.enso.base.enso_cloud;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.http.HttpClient;
import java.net.http.HttpHeaders;
import java.net.http.HttpRequest;
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
import java.util.function.Function;
import java.util.zip.GZIPInputStream;
import org.enso.base.cache.ReloadDetector;
import org.enso.base.cache.ResponseTooLargeException;
import org.enso.base.net.URISchematic;
import org.enso.base.net.URIWithSecrets;
import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;

/** Makes HTTP requests with secrets in either header or query string. */
public final class EnsoSecretHelper extends SecretValueResolver {
  private static Value handleRequestException(Exception e) {
    var wrappedException =
        switch (e) {
          case UnsupportedOperationException unsupportedOperationException ->
              EnsoMeta.makeInstance(
                  "Standard.Base.Errors.Unimplemented",
                  "Unimplemented",
                  "Error",
                  unsupportedOperationException.getMessage());
          case IllegalArgumentException argException ->
              EnsoMeta.makeInstance(
                  "Standard.Base.Errors.Illegal_Argument",
                  "Illegal_Argument",
                  "Error",
                  argException.getMessage(),
                  argException);
          case IOException _ ->
              EnsoMeta.makeInstance(
                  "Standard.Base.Network.HTTP",
                  "Request_Error",
                  "Error",
                  e.getClass().getCanonicalName(),
                  e.getMessage());
          case ResponseTooLargeException tl ->
              EnsoMeta.makeInstance(
                  "Standard.Base.Errors.Common",
                  "Response_Too_Large",
                  "Error",
                  tl.getActualSize(),
                  tl.getLimit());
          default -> null;
        };
    if (wrappedException == null) {
      throw new RuntimeException(e);
    }
    return EnsoMeta.asDataflowError(wrappedException);
  }

  public static Value resolveBody(EnsoRequestBody body, Function<byte[], String> hashFunction) {
    try {
      var publisher = EnsoRequestBody.build(body);

      var hash = "";
      if (hashFunction != null) {
        byte[] bytes = EnsoRequestBody.hashInput(body);
        hash = hashFunction.apply(bytes);
      }

      return EnsoMeta.makeInstance(
          "Standard.Base.Network.HTTP",
          "Resolved_Body",
          "Value",
          publisher,
          EnsoMeta.getNothing(),
          hash);
    } catch (Exception e) {
      return handleRequestException(e);
    }
  }

  private static EnsoHTTPResponseCache cache;

  /**
   * Gets a JDBC connection resolving EnsoKeyValuePair into the properties.
   *
   * @param properties properties in the form of {@code HideableValue.KeyValuePair}
   */
  public static Connection getJDBCConnection(String url, Map<String, EnsoHideableValue> properties)
      throws SQLException {
    var javaProperties = new Properties();
    for (var key : properties.keySet()) {
      HideableValue value = HideableValue.from(properties.get(key));
      // Special handling for PrivateKey parameter.
      if (value instanceof HideableImpl.InterpretAsPrivateKey(HideableValue innerValue)) {
        String rawKey = resolveValue(innerValue);
        PrivateKey privateKey = HideableImpl.InterpretAsPrivateKey.decodePrivateKey(rawKey);
        javaProperties.put(key, privateKey);
      } else {
        javaProperties.setProperty(key, resolveValue(value));
      }
    }

    return DriverManager.getConnection(url, javaProperties);
  }

  /**
   * Gets the actual URI with all secrets resolved, so that it can be used to create a request. This
   * value should never be returned to Enso.
   */
  private static URI resolveURI(String baseUri, List<EnsoHeader> queryParameters) {
    try {
      var resolvedQueryParameters =
          queryParameters.stream()
              .map(
                  p ->
                      new AbstractMap.SimpleEntry<>(
                          p.name(), resolveValue(HideableValue.from(p.as_derived_secret()))))
              .toList();
      var resolvedSchematic = new URISchematic(URI.create(baseUri), resolvedQueryParameters);
      return resolvedSchematic.build();
    } catch (URISyntaxException e) {
      // Here we don't display the message of the exception to avoid risking it may leak any
      // secrets.
      // This should never happen in practice.
      throw new IllegalStateException(
          "Unexpectedly unable to build a valid URI from the base URI: "
              + baseUri
              + ": "
              + e.getClass().getCanonicalName());
    }
  }

  /** Makes a request with secrets in the query string or headers. * */
  public static Value makeRequest(
      HttpClient client,
      String method,
      HttpRequest.BodyPublisher body,
      String baseUri,
      List<EnsoHeader> queryParameters,
      List<EnsoHeader> headers,
      boolean useCache) {
    try {
      var response =
          makeRequestInternal(client, method, body, baseUri, queryParameters, headers, useCache);
      return Value.asValue(response);
    } catch (Exception e) {
      return handleRequestException(e);
    }
  }

  private static EnsoHttpResponse makeRequestInternal(
      HttpClient client,
      String method,
      HttpRequest.BodyPublisher body,
      String baseUri,
      List<EnsoHeader> queryParameters,
      List<EnsoHeader> headers,
      boolean useCache)
      throws IllegalArgumentException,
          IOException,
          InterruptedException,
          ResponseTooLargeException {
    // Clone incoming builder so we can't leak secrets through it
    var builder = HttpRequest.newBuilder().method(method, body);

    // Build a new URI with the query arguments.
    URI resolvedURI = resolveURI(baseUri, queryParameters);

    var resolvedHeaders =
        headers.stream()
            .map(
                header -> {
                  return new AbstractMap.SimpleEntry<>(
                      header.name(), resolveValue(HideableValue.from(header.as_derived_secret())));
                })
            .toList();

    var requestMaker =
        new RequestMaker(
            client,
            builder,
            new URIWithSecrets(baseUri, queryParameters),
            resolvedURI,
            headers,
            resolvedHeaders);

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
    private final List<EnsoHeader> headers;
    private final List<? extends Map.Entry<String, String>> resolvedHeaders;

    RequestMaker(
        HttpClient client,
        Builder builder,
        URIWithSecrets uri,
        URI resolvedURI,
        List<EnsoHeader> headers,
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
          uri.containsSecrets()
              || headers.stream()
                  .anyMatch(p -> HideableValue.from(p.as_derived_secret()).containsSecrets());
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
