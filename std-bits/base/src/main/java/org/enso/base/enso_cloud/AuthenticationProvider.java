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

  public static String getAPIRootURI() {
    var envUri = System.getenv("ENSO_CLOUD_API_URI");
    return envUri == null ? "https://7aqkn3tnbc.execute-api.eu-west-1.amazonaws.com/" : envUri;
  }

  public static void flushCloudCaches() {
    EnsoSecretReader.flushCache();
  }
}
