package org.enso.base.enso_cloud;

import org.enso.base.cache.ReloadDetector;
import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;

public class AuthenticationProvider implements ReloadDetector.HasClearableCache {
  public static AuthenticationProvider INSTANCE = new AuthenticationProvider();

  private AuthenticationProvider() {
    ReloadDetector.register(this);
  }

  public interface AuthenticationService {
    String get_access_token();

    void force_refresh();
  }

  private Value authenticationServiceAsEnso = null;
  private AuthenticationService authenticationServiceAsJava = null;

  public void reset() {
    authenticationServiceAsEnso = null;
    authenticationServiceAsJava = null;
  }

  private Value createAuthenticationService() {
    return EnsoMeta.callStaticModuleMethod(
        "Standard.Base.Enso_Cloud.Internal.Authentication", "instantiate_authentication_service");
  }

  private void ensureServicesSetup() {
    var ensoInstance = createAuthenticationService();
    var javaInstance = ensoInstance.as(AuthenticationService.class);
    authenticationServiceAsEnso = ensoInstance;
    authenticationServiceAsJava = javaInstance;
  }

  AuthenticationService getAuthenticationService() {
    if (authenticationServiceAsJava == null) {
      ensureServicesSetup();
    }

    return authenticationServiceAsJava;
  }

  public Value getAuthenticationServiceEnsoInstance() {
    ReloadDetector.clearOnReload(this);

    if (authenticationServiceAsEnso == null) {
      ensureServicesSetup();
    }

    return authenticationServiceAsEnso;
  }

  public String getAccessToken() {
    ReloadDetector.clearOnReload(this);

    return getAuthenticationService().get_access_token();
  }

  @Override /* HasClearableCache */
  public void clearCache() {
    reset();
  }
}
