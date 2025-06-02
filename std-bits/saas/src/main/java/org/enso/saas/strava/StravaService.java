package org.enso.saas.strava;

import java.io.IOException;
import java.time.ZonedDateTime;

import org.enso.base.enso_cloud.ExternalLibraryCredentialHelper;
import org.enso.base.enso_cloud.ExternalLibraryCredentialHelper.AccessToken;
import org.enso.base.enso_cloud.ExternalLibraryCredentialHelper.CredentialReference;

public final class StravaService {
    private final CredentialReference credentialReference;
    private AccessToken accessToken;

    public StravaService(CredentialReference credentialReference) {
        this.credentialReference = credentialReference;
    }

    private void refresh() throws IOException {
      accessToken = ExternalLibraryCredentialHelper.requestAccessToken(credentialReference);
    }

    // TODO remove this.
    public AccessToken getAccessToken() throws IOException {
      if (accessToken == null) {
        refresh();
      }
      //return new AccessToken("token", ZonedDateTime.now().plusMonths(1));
      return accessToken;
    }
}
