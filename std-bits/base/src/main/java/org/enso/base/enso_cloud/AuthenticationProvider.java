package org.enso.base.enso_cloud;

import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;

public class AuthenticationProvider {

  public interface AuthenticationService {
    String get_access_token();

    void force_refresh();
  }

  private static Value authenticationServiceAsEnso = null;
  private static AuthenticationService authenticationServiceAsJava = null;

  public static void reset() {
    authenticationServiceAsEnso = null;
    authenticationServiceAsJava = null;
  }

  private static Value createAuthenticationService() {
    return EnsoMeta.callStaticModuleMethod(
        "Standard.Base.Enso_Cloud.Internal.Authentication", "instantiate_authentication_service");
  }

  private static void ensureServicesSetup() {
    var ensoInstance = createAuthenticationService();
    var javaInstance = ensoInstance.as(AuthenticationService.class);
    authenticationServiceAsEnso = ensoInstance;
    authenticationServiceAsJava = javaInstance;
  }

  static AuthenticationService getAuthenticationService() {
    if (authenticationServiceAsJava == null) {
      ensureServicesSetup();
    }

    return authenticationServiceAsJava;
  }

  public static Value getAuthenticationServiceEnsoInstance() {
    if (authenticationServiceAsEnso == null) {
      ensureServicesSetup();
    }

    return authenticationServiceAsEnso;
  }

  public static String getAccessToken() {
    return getAuthenticationService().get_access_token();
  }
}
