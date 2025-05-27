package org.enso.microsoft.azure;

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

  record ClientSecret(HideableValue tenantId, HideableValue clientId, HideableValue clientSecret)
      implements AzureCredential {
    @Override
    public String uniqueId() {
      return "ClientSecret("
          + makeUnique(tenantId)
          + ", "
          + makeUnique(clientId)
          + ", "
          + makeUnique(clientSecret)
          + ")";
    }
  }

  private static String makeUnique(HideableValue value) {
    return switch (value) {
      case HideableValue.PlainValue plainValue -> plainValue.value();
      case HideableValue.SecretValue secretID -> "{{Secret:" + secretID.secretId() + "}}";
      case HideableValue.ConcatValues concatValues -> makeUnique(concatValues.left())
          + "_"
          + makeUnique(concatValues.right());
      case HideableValue.Base64EncodeValue base64EncodeValue -> "base64_"
          + makeUnique(base64EncodeValue.value());
      default -> throw new IllegalArgumentException("Unexpected value: " + value);
    };
  }
}
