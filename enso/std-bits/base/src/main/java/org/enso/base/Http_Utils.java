package org.enso.base;

import org.enso.base.net.http.BasicAuthorization;
import org.enso.base.net.http.MultipartBodyBuilder;
import org.enso.base.net.http.UrlencodedBodyBuilder;

import java.net.http.HttpHeaders;
import java.util.AbstractMap;
import java.util.List;
import java.util.Map;

/** Utils for standard HTTP library. */
public class Http_Utils {

  /**
   * Create the header for HTTP basic auth.
   *
   * @param user the user name.
   * @param password the password.
   * @return the new header.
   */
  public static String header_basic_auth(String user, String password) {
    return BasicAuthorization.header(user, password);
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
   * Create the builder for an url-encoded form data.
   *
   * @return the url-encoded form builder.
   */
  public static UrlencodedBodyBuilder urlencoded_body_builder() {
    return new UrlencodedBodyBuilder();
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
