package org.enso.base.enso_cloud;

import org.enso.base.Environment_Utils;

public final class CloudAPI {
  /**
   * Returns the URI to the root of the Cloud API.
   *
   * <p>It always ends with a slash.
   */
  public static String getAPIRootURI() {
    var envUri = Environment_Utils.get_environment_variable("ENSO_CLOUD_API_URI");
    var effectiveUri =
        envUri == null ? "https://7aqkn3tnbc.execute-api.eu-west-1.amazonaws.com/" : envUri;
    var uriWithSlash = effectiveUri.endsWith("/") ? effectiveUri : effectiveUri + "/";
    return uriWithSlash;
  }

  /**
   * Returns the ID of the currently opened cloud project.
   *
   * <p>When running locally, this returns {@code null}.
   */
  public static String getCloudProjectId() {
    return Environment_Utils.get_environment_variable("ENSO_CLOUD_PROJECT_ID");
  }

  /**
   * Returns the session ID of the currently running cloud session.
   *
   * <p>When running locally, this returns {@code null}.
   */
  public static String getCloudSessionId() {
    return Environment_Utils.get_environment_variable("ENSO_CLOUD_PROJECT_SESSION_ID");
  }

  public static void flushCloudCaches() {
    CloudRequestCache.clear();
    AuthenticationProvider.reset();
    EnsoSecretReader.flushCache();
  }
}
