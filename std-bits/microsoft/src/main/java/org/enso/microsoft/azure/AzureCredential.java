package org.enso.microsoft.azure;

import org.enso.base.enso_cloud.HideableValue;

public sealed interface AzureCredential {
  record Default() implements AzureCredential {}

  record Environment() implements AzureCredential {}

  record CLI() implements AzureCredential {}

  record ClientSecret(HideableValue tenantId, HideableValue clientId, HideableValue clientSecret) implements AzureCredential {}
}
