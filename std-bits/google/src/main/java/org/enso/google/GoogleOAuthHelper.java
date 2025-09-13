package org.enso.google;

import com.google.auth.oauth2.AccessToken;
import com.google.auth.oauth2.GoogleCredentials;
import java.io.IOException;
import java.util.Date;
import java.util.List;
import java.util.Map;
import org.enso.base.enso_cloud.ExternalLibraryCredentialHelper;

class GoogleOAuthHelper {
  static GoogleCredentials createCredential(
      ExternalLibraryCredentialHelper.CredentialReference credentialReference) {
    credentialReference.expectForService("Google");
    return new CloudRenewableGoogleCredentials(credentialReference);
  }

  private static class CloudRenewableGoogleCredentials extends GoogleCredentials {
    private final ExternalLibraryCredentialHelper.CredentialReference credentialReference;
    private AccessToken token = null;

    public CloudRenewableGoogleCredentials(
        ExternalLibraryCredentialHelper.CredentialReference credentialReference) {
      this.credentialReference = credentialReference;
    }

    @Override
    public void refresh() throws IOException {
      var accessToken = ExternalLibraryCredentialHelper.requestAccessToken(credentialReference);
      if (accessToken.expirationDate() == null) {
        throw new IllegalStateException(
            "The refresh endpoint did not return required expiration date for the Google access"
                + " token.");
      }

      Date expiration = Date.from(accessToken.expirationDate().toInstant());
      token = new AccessToken(accessToken.token(), expiration);
    }

    @Override
    public AccessToken refreshAccessToken() throws IOException {
      refresh();
      return token;
    }

    @Override
    public Map<String, List<String>> getRequestMetadata() throws IOException {
      if (token == null) {
        refresh();
      }

      return Map.of("Authorization", List.of("Bearer " + token.getTokenValue()));
    }
  }
}
