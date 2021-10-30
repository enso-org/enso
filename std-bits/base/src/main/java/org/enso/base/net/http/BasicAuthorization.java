package org.enso.base.net.http;

import java.nio.charset.StandardCharsets;
import java.util.Base64;

/** An authenticator for HTTP basic auth. */
public final class BasicAuthorization {

  /**
   * Build HTTP basic authorization header.
   *
   * @param user the user name.
   * @param password the password
   * @return return base64 encoded header for HTTP basic auth.
   */
  public static String header(String user, String password) {
    String auth = user + ":" + password;
    String authEncoded = Base64.getEncoder().encodeToString(auth.getBytes(StandardCharsets.UTF_8));
    return "Basic " + authEncoded;
  }
}
