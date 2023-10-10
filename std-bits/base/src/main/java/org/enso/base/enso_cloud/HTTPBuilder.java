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
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;

/***
 * Makes HTTP requests with secrets in either header or query string.
 */
public class HTTPBuilder {
  private static String makeQueryAry(EnsoKeyValuePair pair) {
    String resolvedValue = (pair.secretId() != null && !pair.secretId().isBlank())
        ? EnsoSecretReader.readSecret(pair.secretId())
        : pair.value();

    String resolvedKey = pair.key() != null && !pair.key().isBlank() ? URLEncoder.encode(pair.key(), StandardCharsets.UTF_8) + "=" : "";
    return resolvedKey + URLEncoder.encode(resolvedValue, StandardCharsets.UTF_8);
  }

  public static Properties resolve(EnsoKeyValuePair[] properties) {
    var javaProperties = new Properties();
    for (EnsoKeyValuePair(
        String key, String value, String secretId
    ) : properties) {
      var resolvedValue = (secretId != null && !secretId.isBlank())
          ? EnsoSecretReader.readSecret(secretId)
          : value;
      javaProperties.setProperty(key, resolvedValue);
    }
    return javaProperties;
  }

  //** Makes a request with secrets in the query string or headers. **//
  public static HttpResponse<InputStream> makeRequest(HttpClient client, Builder builder, URI uri, List<EnsoKeyValuePair> queryArguments, List<EnsoKeyValuePair> headerArguments)
      throws IOException, InterruptedException {
    // Build a new URI with the query arguments.
    if (queryArguments != null && !queryArguments.isEmpty()) {
      var baseQuery = uri.getQuery();
      var query = queryArguments.stream().map(HTTPBuilder::makeQueryAry).collect(Collectors.joining("&"));
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
      for (EnsoKeyValuePair(
          String key, String value, String secretId
      ) : headerArguments) {
        var resolvedValue = (secretId != null && !secretId.isBlank())
            ? EnsoSecretReader.readSecret(secretId)
            : value;
        builder.header(key, resolvedValue);
      }
    }

    // Build and Send the request.
    var httpRequest = builder.build();
    var bodyHandler = HttpResponse.BodyHandlers.ofInputStream();
    return client.send(httpRequest, bodyHandler);
  }
}
