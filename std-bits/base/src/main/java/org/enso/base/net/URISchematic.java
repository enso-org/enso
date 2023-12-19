package org.enso.base.net;

import org.graalvm.collections.Pair;

import java.net.IDN;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.List;

/**
 * A raw schematic that may be used to build a URI.
 * <p>
 * It describes how a base URI gets overridden with added query parameters or other options like user-info.
 * <p>
 * This is the common entry point for building a URI with or without secrets.
 */
public record URISchematic(URI baseUri, List<Pair<String, String>> queryParameters, Pair<String, String> userInfo) {
  public URI build() throws URISyntaxException {
    StringBuilder uriBuilder = new StringBuilder();
    uriBuilder.append(baseUri.getScheme()).append("://");

    if (userInfo != null) {
      var username = userInfo.getLeft();
      var password = userInfo.getRight();
      if (username.contains(":")) {
        throw new IllegalArgumentException("Username within an URI cannot contain ':'.");
      }

      uriBuilder.append(encode(username)).append(":").append(encode(password)).append("@");
    } else if (baseUri.getRawUserInfo() != null) {
      uriBuilder.append(baseUri.getRawUserInfo()).append("@");
    }

    String rawAuthority = baseUri.getRawAuthority();
    if (rawAuthority == null) {
      throw new IllegalArgumentException("URI must have an authority.");
    }

    int atLocation = rawAuthority.indexOf('@');
    String hostAndPort = atLocation < 0 ? rawAuthority : rawAuthority.substring(atLocation + 1);
    uriBuilder.append(IDN.toASCII(hostAndPort));

    String path = baseUri.getRawPath();
    String queryPart = buildQueryPart();
    String fragment = baseUri.getRawFragment();

    if (path != null && !path.isEmpty()) {
      uriBuilder.append(path);
    } else if (queryPart != null || fragment != null) {
      // If we had no path, but we do have a query or a fragment, we need to add a / to precede the ? or #.
      uriBuilder.append("/");
    }

    if (queryPart != null) {
      uriBuilder.append("?").append(queryPart);
    }

    if (fragment != null) {
      uriBuilder.append("#").append(fragment);
    }

    return new URI(uriBuilder.toString());
  }

  private String buildQueryPart() {
    StringBuilder queryBuilder = new StringBuilder();
    if (baseUri.getRawQuery() != null) {
      queryBuilder.append(baseUri.getRawQuery());
    }

    for (var param : queryParameters) {
      if (!queryBuilder.isEmpty()) {
        queryBuilder.append("&");
      }

      String name = param.getLeft();
      String value = param.getRight();
      queryBuilder.append(encode(name)).append("=").append(encode(value));
    }

    if (queryBuilder.isEmpty()) {
      return null;
    } else {
      return queryBuilder.toString();
    }
  }

  private static String encode(String value) {
    return URLEncoder.encode(value, StandardCharsets.UTF_8);
  }
}
