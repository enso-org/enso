package org.enso.base.enso_cloud;

public class AuthenticationProvider {
  private static String token;

  public static String setToken(String token) {
    AuthenticationProvider.token = token;
    return AuthenticationProvider.token;
  }

  public static String getToken() {
    return AuthenticationProvider.token;
  }
}
