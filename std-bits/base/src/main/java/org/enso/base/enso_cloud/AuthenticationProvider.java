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
    var effectiveUri =
        envUri == null ? "https://7aqkn3tnbc.execute-api.eu-west-1.amazonaws.com/" : envUri;
    var uriWithSlash = effectiveUri.endsWith("/") ? effectiveUri : effectiveUri + "/";
    return uriWithSlash;
  }

  public record CloudWorkingDirectory(String name, String id, String organizationId) {}

  public static CloudWorkingDirectory getCurrentWorkingDirectory() {
    if (cachedWorkingDirectory != null) {
      return cachedWorkingDirectory;
    }

    String directoryId = Environment_Utils.get_environment_variable("ENSO_PROJECT_PATH");
    if (directoryId == null) {
      // No current working directory is set
      return null;
    }

    // TODO we should be able to fetch the name and organizationId from the cloud:
    String directoryName = "???";
    String organizationId = "";
    cachedWorkingDirectory = new CloudWorkingDirectory(directoryName, directoryId, organizationId);
    return cachedWorkingDirectory;
  }

  private static CloudWorkingDirectory cachedWorkingDirectory = null;

  public static void flushCloudCaches() {
    EnsoSecretReader.flushCache();
    cachedWorkingDirectory = null;
  }
}
