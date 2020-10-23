package org.enso.base.net.http;

import java.net.Authenticator;
import java.net.PasswordAuthentication;

/** An authenticator for HTTP basic auth. */
public final class BasicAuthenticator extends Authenticator {

  private final String user;
  private final String password;

  public BasicAuthenticator(String user, String password) {
    this.user = user;
    this.password = password;
  }

  @Override
  protected PasswordAuthentication getPasswordAuthentication() {
    return new PasswordAuthentication(user, password.toCharArray());
  }
}
