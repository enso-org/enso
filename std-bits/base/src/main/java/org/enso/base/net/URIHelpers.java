package org.enso.base.net;

import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.List;

public class URIHelpers {
  public record NameValuePair(String name, String value) {}

  public static URI addQueryParameters(URI uri, List<NameValuePair> params)
      throws URISyntaxException {
    StringBuilder query = new StringBuilder();
    if (uri.getRawQuery() != null) {
      query.append(uri.getRawQuery());
    }

    for (NameValuePair param : params) {
      if (!query.isEmpty()) {
        query.append("&");
      }

      query.append(encode(param.name)).append("=").append(encode(param.value));
    }

    StringBuilder uriBuilder = new StringBuilder();
    uriBuilder
        .append(uri.getScheme())
        .append("://")
        .append(uri.getRawAuthority())
        .append(uri.getRawPath());
    if (!query.isEmpty()) {
      uriBuilder.append("?").append(query);
    }

    if (uri.getRawFragment() != null) {
      uriBuilder.append("#").append(uri.getRawFragment());
    }

    return new URI(uriBuilder.toString());
  }

  private static String encode(String value) {
    return URLEncoder.encode(value, StandardCharsets.UTF_8);
  }
}
