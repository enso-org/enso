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

  public static void flushCloudCaches() {
    AuthenticationProvider.reset();
    EnsoSecretReader.flushCache();
  }
}
