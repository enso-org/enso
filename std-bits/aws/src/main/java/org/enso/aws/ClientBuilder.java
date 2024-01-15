package org.enso.aws;

import org.enso.base.enso_cloud.ExternalLibrarySecretHelper;
import org.enso.base.enso_cloud.HideableValue;

public class ClientBuilder {
  private String unsafeResolveSecrets(HideableValue value) {
    return ExternalLibrarySecretHelper.resolveValue(value);
  }
}
