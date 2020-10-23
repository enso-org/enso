package org.enso.base;

import org.enso.base.net.http.BasicAuthenticator;
import org.enso.base.net.http.MultipartBodyBuilder;

import java.net.Authenticator;
import java.net.http.HttpHeaders;
import java.util.AbstractMap;
import java.util.List;
import java.util.Map;

/** Utils for standard HTTP library. */
public class Http_Utils {

  /**
   * Create the authenticator for HTTP basic auth.
   *
   * @param user the user name.
   * @param password the password.
   * @return the new basic authenticator.
   */
  public static Authenticator basic_authenticator(String user, String password) {
    return new BasicAuthenticator(user, password);
  }

  /**
   * Create the builder for a multipart form data.
   *
   * @return the multipart form builder.
   */
  public static MultipartBodyBuilder multipart_body_builder() {
    return new MultipartBodyBuilder();
  }

  /**
   * Get HTTP response headers as a list of map entries.
   *
   * @param headers HTTP response headers.
   * @return the key-value list of headers.
   */
  public static Object[] get_headers(HttpHeaders headers) {
    Map<String, List<String>> map = headers.map();
    return map.keySet().stream()
        .flatMap(k -> map.get(k).stream().map(v -> new AbstractMap.SimpleImmutableEntry<>(k, v)))
        .toArray();
  }
}
