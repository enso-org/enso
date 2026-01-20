package org.enso.base.enso_cloud;

import java.util.List;

/**
 * An entry point allowing external libraries to access Enso secrets.
 *
 * <p>It will only allow access from trusted code locations.
 */
public final class ExternalLibrarySecretHelper extends SecretValueResolver {
  public static String resolveValue(HideableValue hideableValue) throws EnsoSecretAccessDenied {
    RestrictedAccess.checkAccess(allowedAccessLocations);
    return SecretValueResolver.resolveValue(hideableValue);
  }

  private static final List<RestrictedAccess.AccessLocation> allowedAccessLocations =
      List.of(
          new RestrictedAccess.AccessLocation("org.enso.aws.ClientBuilder", "unsafeResolveSecrets"),
          new RestrictedAccess.AccessLocation(
              "org.enso.microsoft.azure.CredentialHelper", "unsafeResolveSecrets"),
          new RestrictedAccess.AccessLocation(
              "org.enso.saas.CredentialSetter", "unsafeResolveSecrets"));
}
