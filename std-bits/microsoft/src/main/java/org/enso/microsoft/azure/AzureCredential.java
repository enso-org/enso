package org.enso.microsoft.azure;

import com.azure.core.credential.TokenCredential;

/**
 * This class is used by Enso to create a credential object for Microsoft Azure.
 */
public final class AzureCredential {
  /**
   * Creates a credential object using the default Azure credential builder.
   *
   * @return a TokenCredential object.
   */
  public static TokenCredential fromDefault() {
    // ToDo: If the default credential is not found, gets stuck in a loop.
    return new com.azure.identity.DefaultAzureCredentialBuilder().build();
  }

  /**
   * Creates a credential object using the client secret credential builder.
   *
   * @param tenantId the tenant ID.
   * @param clientId the client ID.
   * @param clientSecret the client secret.
   * @return a TokenCredential object.
   */
  public static TokenCredential fromClientSecret(String tenantId, String clientId, String clientSecret) {
    return new com.azure.identity.ClientSecretCredentialBuilder()
        .clientId(clientId)
        .clientSecret(clientSecret)
        .tenantId(tenantId)
        .build();
  }
}
