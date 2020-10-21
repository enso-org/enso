package org.enso.base;

import java.net.Authenticator;
import java.net.PasswordAuthentication;

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

  public static Authenticator basic_authenticator(String user, String password) {
    return new BasicAuthenticator(user, password);
  }

  public static byte[] byte_array(int lenght) {
    return new byte[lenght];
  }

}
