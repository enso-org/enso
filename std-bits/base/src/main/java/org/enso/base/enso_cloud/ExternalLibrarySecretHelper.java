package org.enso.base.enso_cloud;

import java.util.List;

public final class ExternalLibrarySecretHelper extends SecretValueResolver {
  public static String resolveValue(HideableValue hideableValue) throws EnsoSecretAccessDenied {
    checkAccess();
    return SecretValueResolver.resolveValue(hideableValue);
  }

  private static void checkAccess() throws EnsoSecretAccessDenied {

  }

  private record AccessLocation(String className, String method) {
  }

  private static final List<AccessLocation> allowedAccessLocations = List.of();
}
