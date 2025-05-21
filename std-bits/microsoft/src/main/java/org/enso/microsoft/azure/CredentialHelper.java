package org.enso.microsoft.azure;

import com.azure.core.credential.TokenCredential;
import com.azure.identity.AzureCliCredentialBuilder;
import com.azure.identity.ClientSecretCredentialBuilder;
import com.azure.identity.DefaultAzureCredentialBuilder;
import com.azure.identity.EnvironmentCredentialBuilder;
import org.enso.base.enso_cloud.ExternalLibrarySecretHelper;
import org.enso.base.enso_cloud.HideableValue;

/**
 * A helper class to convert {@link AzureCredential} to {@link TokenCredential}.
 *
 * <p>This class is allowed access to secrets. Extra care should be taken to ensure its result is not
 * leaked.
 */
final class CredentialHelper {
  static TokenCredential toTokenCredential(AzureCredential credential) {
    return switch (credential) {
      case AzureCredential.Default() ->
          new DefaultAzureCredentialBuilder().build();
      case AzureCredential.Environment() ->
          new EnvironmentCredentialBuilder().build();
      case AzureCredential.CLI() ->
          new AzureCliCredentialBuilder().build();
      case AzureCredential.ClientSecret(HideableValue tenantId, HideableValue clientId, HideableValue clientSecret) ->
          new ClientSecretCredentialBuilder()
              .tenantId(unsafeResolveSecrets(tenantId))
              .clientId(unsafeResolveSecrets(clientId))
              .clientSecret(unsafeResolveSecrets(clientSecret))
              .build();
    };
  }

  /**
   * This function is allowed access to secrets. Extra care should be taken to ensure its result is
   * not leaked.
   */
  private static String unsafeResolveSecrets(HideableValue value) {
    return ExternalLibrarySecretHelper.resolveValue(value);
  }
}
