package org.enso.base;

import java.net.Authenticator;
import java.net.PasswordAuthentication;
import java.net.http.HttpHeaders;
import java.util.AbstractMap;
import java.util.List;
import java.util.Map;

/** Utils for standard HTTP library. */
public class Http_Utils {

  /** Authenticator for basic auth. */
  private static class BasicAuthenticator extends Authenticator {

    private final String user;
    private final String password;

    BasicAuthenticator(String user, String password) {
      this.user = user;
      this.password = password;
    }

    @Override
    protected PasswordAuthentication getPasswordAuthentication() {
      return new PasswordAuthentication(user, password.toCharArray());
    }
  }

  /**
   * Create an authenticator for http basic auth.
   *
   * @param user the user name.
   * @param password the password.
   * @return a new basic authenticator.
   */
  public static Authenticator basic_authenticator(String user, String password) {
    return new BasicAuthenticator(user, password);
  }

  /**
   * Get the response headers as a list of map entries.
   *
   * @param headers the http headers.
   * @return the key-value list of headers.
   */
  public static Object[] get_headers(HttpHeaders headers) {
    Map<String, List<String>> map = headers.map();
    return map.keySet().stream()
        .flatMap(k -> map.get(k).stream().map(v -> new AbstractMap.SimpleImmutableEntry<>(k, v)))
        .toArray();
  }
}
