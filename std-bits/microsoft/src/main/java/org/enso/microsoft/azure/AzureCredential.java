package org.enso.microsoft.azure;

import org.enso.base.enso_cloud.EnsoHideableValue;
import org.enso.base.enso_cloud.HideableValue;

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

  record ClientSecret(EnsoHideableValue tenantId, EnsoHideableValue clientId, EnsoHideableValue clientSecret)
      implements AzureCredential {
    @Override
    public String uniqueId() {
      var tenantIdStr = HideableValue.from(tenantId).toString();
      var clientIdStr = HideableValue.from(clientId).toString();
      var clientSecretStr = HideableValue.from(clientSecret).toString();
      return "ClientSecret(" + tenantIdStr + ", " + clientIdStr + ", " + clientSecretStr + ")";
    }
  }

  record BlobStorageSASToken(HideableValue token) implements AzureCredential {
    @Override
    public String uniqueId() {
      return "BlobStorageSASToken(" + token + ")";
    }
  }
}
