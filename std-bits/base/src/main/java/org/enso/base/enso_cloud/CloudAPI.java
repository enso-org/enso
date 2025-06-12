package org.enso.base.enso_cloud;

import org.enso.base.Environment_Utils;
import org.enso.base.enso_cloud.audit.AuditLog;

public final class CloudAPI {
  /**
   * Returns the URI to the root of the Cloud API.
   *
   * <p>It always ends with a slash.
   */
  public static String getAPIRootURI() {
    var envUrl = Environment_Utils.get_environment_variable("ENSO_CLOUD_API_URL");
    var effectiveUrl = envUrl == null ? "https://api.cloud.enso.org/" : envUrl;
    var urlWithSlash = effectiveUrl.endsWith("/") ? effectiveUrl : effectiveUrl + "/";
    return urlWithSlash;
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
    CloudRequestCache.INSTANCE.clear();
    AuthenticationProvider.INSTANCE.reset();
    EnsoSecretReader.INSTANCE.flushCache();
    AuditLog.resetCache();
  }
}
