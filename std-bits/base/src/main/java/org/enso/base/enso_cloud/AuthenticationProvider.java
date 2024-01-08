package org.enso.base.enso_cloud;

import org.enso.base.Environment_Utils;

public class AuthenticationProvider {
  private static String token = null;

  public static String setToken(String token) {
    AuthenticationProvider.token = token;
    return AuthenticationProvider.token;
  }

  public static String getToken() {
    return AuthenticationProvider.token;
  }

  public static String getAPIRootURI() {
    var envUri = Environment_Utils.get_environment_variable("ENSO_CLOUD_API_URI");
    return envUri == null ? "https://7aqkn3tnbc.execute-api.eu-west-1.amazonaws.com/" : envUri;
  }

  public static void flushCloudCaches() {
    EnsoSecretReader.flushCache();
  }
}
