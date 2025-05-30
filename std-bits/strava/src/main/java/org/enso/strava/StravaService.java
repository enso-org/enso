package org.enso.strava;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import org.enso.base.enso_cloud.ExternalLibraryCredentialHelper;
import org.enso.base.enso_cloud.ExternalLibraryCredentialHelper.AccessToken;
import org.enso.base.enso_cloud.ExternalLibraryCredentialHelper.CredentialReference;

public class StravaService {
    private final CredentialReference credentialReference;
    private AccessToken accessToken;

    public StravaService(CredentialReference credentialReference) {
        this.credentialReference = credentialReference;
    }

    private void refresh() throws IOException {
      accessToken = ExternalLibraryCredentialHelper.requestAccessToken(credentialReference);
    }

    public Map<String, List<String>> getRequestHeaders() throws IOException {
      if (accessToken == null) {
        refresh();
      }

      // TODO this is the wrong format.
      return Map.of("Authorization", List.of("Bearer " + accessToken));
    }
}
