package org.enso.microsoft.azure;

import org.enso.base.enso_cloud.EnsoHideableValue;

public sealed interface AzureCredential {
  String uniqueId();

  record Default() implements AzureCredential {
    @Override
    public String uniqueId() {
      return "Default";
    }
  }

  /**
   * Represents an Azure credential that uses the environment variables to authenticate. The
   * required variables are: `AZURE_TENANT_ID`, `AZURE_CLIENT_ID`, `AZURE_CLIENT_SECRET`
   */
  record Environment() implements AzureCredential {
    @Override
    public String uniqueId() {
      return "Environment";
    }
  }

  record CLI() implements AzureCredential {
    @Override
    public String uniqueId() {
      return "CLI";
    }
  }

  record ClientSecret(
      EnsoHideableValue tenantId, EnsoHideableValue clientId, EnsoHideableValue clientSecret)
      implements AzureCredential {
    @Override
    public String uniqueId() {
      return "ClientSecret("
          + tenantId.uniqueId()
          + ", "
          + clientId.uniqueId()
          + ", "
          + clientSecret.uniqueId()
          + ")";
    }
  }

  record BlobStorageSASToken(EnsoHideableValue token) implements AzureCredential {
    @Override
    public String uniqueId() {
      return "BlobStorageSASToken(" + token.uniqueId() + ")";
    }
  }
}
