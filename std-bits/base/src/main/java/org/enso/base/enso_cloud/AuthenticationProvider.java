package org.enso.base.enso_cloud;

import org.graalvm.polyglot.Context;
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
    var context = Context.getCurrent().getBindings("enso");
    var module = context.invokeMember("get_module", "Standard.Base.Enso_Cloud.Internal.Authentication");
    var moduleType = module.invokeMember("get_associated_type");
    var factory = module.invokeMember("get_method", moduleType, "instantiate_authentication_service");
    // The static method takes the module as the synthetic 'self' argument.
    return factory.execute(moduleType);
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
