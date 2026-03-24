package org.enso.saas.salesforce;

import com.fasterxml.jackson.databind.JsonNode;
import org.enso.base.enso_cloud.ExternalLibraryCredentialHelper;
import org.enso.base.enso_cloud.ExternalLibraryCredentialHelper.CredentialReference;

public final class SalesforceCloudCredentials {
  private static SalesforceRefreshToken unsafeReadCredential(
      CredentialReference credentialReference) {
    credentialReference.expectForService("Salesforce");
    ExternalLibraryCredentialHelper.CredentialConfig config =
        ExternalLibraryCredentialHelper.readCredential(credentialReference);
    return SalesforceRefreshToken.parse(config.refreshToken());
  }

  public static String getInstanceUrl(CredentialReference credentialReference) {
    return unsafeReadCredential(credentialReference).instanceUrl();
  }

  private record SalesforceRefreshToken(String instanceUrl) {
    private static SalesforceRefreshToken parse(
        ExternalLibraryCredentialHelper.RefreshToken token) {
      JsonNode metadata = token.metadata();
      if (metadata == null || !metadata.isObject()) {
        throw ExternalLibraryCredentialHelper.malformedCredential();
      }

      var instanceUrlField = metadata.get("instanceUrl");

      if (instanceUrlField == null || !instanceUrlField.isTextual()) {
        throw ExternalLibraryCredentialHelper.malformedCredential();
      }

      return new SalesforceRefreshToken(instanceUrlField.asText());
    }
  }
}
