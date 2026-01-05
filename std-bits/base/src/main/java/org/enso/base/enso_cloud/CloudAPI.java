package org.enso.base.enso_cloud;

import org.enso.base.enso_cloud.audit.AuditLog;
import org.enso.base.polyglot.EnsoMeta;

public final class CloudAPI {
  private CloudAPI() {}

  /**
   * Returns the URI to the root of the Cloud API.
   *
   * <p>It always ends with a slash.
   */
  public static String getAPIRootURI() {
    var envUrl =
        EnsoMeta.callStaticModuleMethod(
            "Standard.Base.System.Environment", "get", "ENSO_CLOUD_API_URL");
    var effectiveUrl = envUrl.isNull() ? "https://api.cloud.enso.org/" : envUrl.asString();
    var urlWithSlash = effectiveUrl.endsWith("/") ? effectiveUrl : effectiveUrl + "/";
    return urlWithSlash;
  }

  /**
   * Returns the ID of the currently opened cloud project.
   *
   * <p>When running locally, this returns {@code null}.
   */
  public static String getCloudProjectId() {
    var id =
        EnsoMeta.callStaticModuleMethod(
            "Standard.Base.System.Environment", "get", "ENSO_CLOUD_PROJECT_ID");
    return id.isNull() ? null : id.asString();
  }

  /**
   * Returns the session ID of the currently running cloud session.
   *
   * <p>When running locally, this returns {@code null}.
   */
  public static String getCloudSessionId() {
    var id =
        EnsoMeta.callStaticModuleMethod(
            "Standard.Base.System.Environment", "get", "ENSO_CLOUD_PROJECT_SESSION_ID");
    return id.isNull() ? null : id.asString();
  }

  public static void flushCloudCaches() {
    CloudRequestCache.INSTANCE.clear();
    AuthenticationProvider.INSTANCE.reset();
    EnsoSecretReader.INSTANCE.flushCache();
    AuditLog.resetCache();
  }
}
