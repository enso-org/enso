package org.enso.microsoft.azure;

import com.azure.core.credential.TokenCredential;
import com.azure.core.exception.ClientAuthenticationException;
import com.azure.identity.AzureCliCredentialBuilder;
import com.azure.identity.ClientSecretCredentialBuilder;
import com.azure.identity.DefaultAzureCredentialBuilder;
import com.azure.identity.EnvironmentCredentialBuilder;
import org.enso.base.enso_cloud.ExternalLibrarySecretHelper;
import org.enso.base.enso_cloud.HideableValue;

/**
 * A helper class to convert {@link AzureCredential} to {@link TokenCredential}.
 *
 * <p>This class is allowed access to secrets. Extra care should be taken to ensure its result is
 * not leaked.
 */
final class CredentialHelper {
  static TokenCredential toTokenCredential(AzureCredential credential) {
    return switch (credential) {
      case AzureCredential.Default() -> new DefaultAzureCredentialBuilder().build();
      case AzureCredential.Environment() -> {
        if (!isPresent(System.getenv("AZURE_CLIENT_ID"))
            || !isPresent(System.getenv("AZURE_CLIENT_SECRET"))
            || !isPresent(System.getenv("AZURE_TENANT_ID"))) {
          throw new ClientAuthenticationException(
              "Environment variables AZURE_CLIENT_ID, AZURE_CLIENT_SECRET, and AZURE_TENANT_ID are"
                  + " not all set.",
              null);
        }
        yield new EnvironmentCredentialBuilder().build();
      }
      case AzureCredential.CLI() -> new AzureCliCredentialBuilder().build();
      case AzureCredential.ClientSecret(
              HideableValue tenantId,
              HideableValue clientId,
              HideableValue clientSecret) -> {
        var resolvedTenantId = unsafeResolveSecrets(tenantId);
        var resolvedClientId = unsafeResolveSecrets(clientId);
        var resolvedClientSecret = unsafeResolveSecrets(clientSecret);
        if (!isPresent(resolvedClientId)
            || !isPresent(resolvedClientSecret)
            || !isPresent(resolvedTenantId)) {
          throw new ClientAuthenticationException(
              "Client ID, Client Secret, and Tenant ID are not all set.", null);
        }
        yield new ClientSecretCredentialBuilder()
            .tenantId(resolvedTenantId)
            .clientId(resolvedClientId)
            .clientSecret(resolvedClientSecret)
            .build();
      }
      case AzureCredential.BlobStorageSASToken(HideableValue token) ->
          throw new ClientAuthenticationException(
              "Blob Storage SAS Token is not supported for authentication.", null);
    };
  }

  static String toSASToken(AzureCredential credential) {
    return switch (credential) {
      case AzureCredential.BlobStorageSASToken(HideableValue token) -> unsafeResolveSecrets(token);
      default ->
          throw new IllegalArgumentException(
              "Only BlobStorageSASToken credentials can provide a SAS token.");
    };
  }

  private static boolean isPresent(String value) {
    return value != null && !value.isEmpty();
  }

  /**
   * This function is allowed access to secrets. Extra care should be taken to ensure its result is
   * not leaked.
   */
  private static String unsafeResolveSecrets(HideableValue value) {
    return ExternalLibrarySecretHelper.resolveValue(value);
  }
}
