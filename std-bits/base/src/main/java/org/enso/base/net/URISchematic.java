package org.enso.base.net;

import java.net.IDN;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import org.graalvm.collections.Pair;

/**
 * A raw schematic that may be used to build a URI.
 *
 * <p>It describes how a base URI gets overridden with added query parameters or other options like
 * user-info.
 *
 * <p>This is the common entry point for building a URI with or without secrets.
 */
public record URISchematic(URI baseUri, List<Pair<String, String>> queryParameters) {
  public URI build() throws URISyntaxException {
    StringBuilder authorityBuilder = new StringBuilder();
    if (baseUri.getRawUserInfo() != null) {
      authorityBuilder.append(baseUri.getRawUserInfo()).append("@");
    }

    String rawAuthority = baseUri.getRawAuthority();
    if (rawAuthority != null) {
      int atLocation = rawAuthority.indexOf('@');
      String hostAndPort = atLocation < 0 ? rawAuthority : rawAuthority.substring(atLocation + 1);
      authorityBuilder.append(IDN.toASCII(hostAndPort));
    } else {
      boolean hasUserInfo = !authorityBuilder.isEmpty();
      if (hasUserInfo) {
        throw new IllegalArgumentException(
            "Cannot build an URI with user-info, but without authority.");
      }
    }

    return URITransformer.buildUriFromParts(
        baseUri.getScheme(),
        authorityBuilder.toString(),
        baseUri.getRawPath(),
        buildQueryPart(),
        baseUri.getRawFragment());
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
      queryBuilder
          .append(URITransformer.encodeForQuery(name))
          .append("=")
          .append(URITransformer.encodeForQuery(value));
    }

    if (queryBuilder.isEmpty()) {
      return null;
    } else {
      return queryBuilder.toString();
    }
  }
}
