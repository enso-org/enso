package org.enso.base.net;

import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.List;

/** Utilities for building and transforming URIs. */
public class URITransformer {
  /**
   * Parses a URI from a string. Replaces spaces with %20 to ensure valid encoding.
   *
   * @param uri the URI string to parse
   * @return the parsed URI
   */
  public static URI parse(String uri) throws URISyntaxException {
    int index = uri.indexOf('?');
    if (index >= 0) {
      String uriWithoutQuery = uri.substring(0, index + 1).replaceAll(" ", "%20");
      uri = uriWithoutQuery + uri.substring(index + 1);
    } else {
      uri = uri.replaceAll(" ", "%20");
    }
    var url = new URI(uri);
    // URI class does not encode the path if single argument constructor is used.
    if (url.getPath().equals(url.getRawPath())) {
      return url;
    }

    index = uri.indexOf(url.getRawPath());
    var newUriString =
        uri.substring(0, index)
            + url.getPath().replace(" ", "%20")
            + uri.substring(index + url.getRawPath().length());
    return new URI(newUriString);
  }

  /** Removes query parameters from the given URI. */
  public static URI removeQueryParameters(URI uri) {
    return buildUriFromParts(
        uri.getScheme(), uri.getRawAuthority(), uri.getRawPath(), null, uri.getRawFragment());
  }

  /** Extends the path within a URI with a list of segments. */
  public static URI extendPath(URI uri, List<String> segments) {
    StringBuilder newPath = new StringBuilder();
    String basePath = uri.getRawPath();
    if (basePath == null || basePath.isEmpty()) {
      basePath = "/";
    }

    newPath.append(basePath);
    if (!basePath.endsWith("/")) {
      newPath.append("/");
    }

    for (int i = 0; i < segments.size(); i++) {
      if (i > 0) {
        newPath.append("/");
      }
      newPath.append(encode(segments.get(i)));
    }

    return buildUriFromParts(
        uri.getScheme(),
        uri.getRawAuthority(),
        newPath.toString(),
        uri.getRawQuery(),
        uri.getRawFragment());
  }

  /** Builds a URI from raw parts, allowing some of them to be missing. */
  public static URI buildUriFromParts(
      String scheme, String authority, String path, String query, String fragment) {
    StringBuilder sb = new StringBuilder();
    if (scheme != null) {
      sb.append(scheme);
      sb.append(":");
    }
    sb.append("//");
    if (authority != null) {
      sb.append(authority);
    }

    if (path != null && !path.isEmpty()) {
      sb.append(path);
    } else if (query != null || fragment != null) {
      // If we had no path, but we do have a query or a fragment, we need to add a / to precede the
      // ? or #.
      sb.append("/");
    }

    if (query != null) {
      sb.append("?").append(query);
    }

    if (fragment != null) {
      sb.append("#").append(fragment);
    }

    return URI.create(sb.toString());
  }

  public static String encodeForQuery(String value) {
    return URLEncoder.encode(value, StandardCharsets.UTF_8);
  }

  public static String encode(String value) {
    return URLEncoder.encode(value, StandardCharsets.UTF_8).replace("+", "%20");
  }

  public static String encodeQuery(String rawQueryString) {
    var parts = rawQueryString.split("&");
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < parts.length; i++) {
      if (i > 0) {
        sb.append("&");
      }
      var keyValue = parts[i].split("=", 2);
      sb.append(encodeForQuery(keyValue[0]));
      if (keyValue.length > 1) {
        sb.append("=");
        sb.append(encodeForQuery(keyValue[1]));
      }
    }
    return sb.toString();
  }
}
