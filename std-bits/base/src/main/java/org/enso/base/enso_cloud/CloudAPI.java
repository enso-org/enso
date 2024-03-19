package org.enso.base.enso_cloud;

import org.enso.base.Environment_Utils;

public class CloudAPI {
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
    // To be done in https://github.com/enso-org/enso/issues/9289
    String directoryName = "???";
    String organizationId = "";
    cachedWorkingDirectory = new CloudWorkingDirectory(directoryName, directoryId, organizationId);
    return cachedWorkingDirectory;
  }

  private static CloudWorkingDirectory cachedWorkingDirectory = null;

  public static void flushCloudCaches() {
    AuthenticationProvider.reset();

    cachedWorkingDirectory = null;
    EnsoSecretReader.flushCache();
  }
}
