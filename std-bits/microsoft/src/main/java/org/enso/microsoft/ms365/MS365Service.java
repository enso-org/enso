package org.enso.microsoft.ms365;

import java.io.IOException;
import java.time.ZonedDateTime;
import org.enso.base.enso_cloud.ExternalLibraryCredentialHelper;
import org.enso.base.enso_cloud.ExternalLibraryCredentialHelper.AccessToken;
import org.enso.base.enso_cloud.ExternalLibraryCredentialHelper.CredentialReference;

public final class MS365Service {
  // We refresh a token if it will expire less than this far in the future.
  private static final long CLOSE_TO_EXPIRATION_MINUTES = 5;

  private final CredentialReference credentialReference;
  private AccessToken accessToken;

  public MS365Service(CredentialReference credentialReference) {
    this.credentialReference = credentialReference;
  }

  private void refresh() throws IOException {
    accessToken = ExternalLibraryCredentialHelper.requestAccessToken(credentialReference);
  }

  // True if we have no token or we have one but it's close to expiring.
  private boolean shouldRefresh() {
    return accessToken == null || closeToExpiring(accessToken);
  }

  public AccessToken getAccessToken() throws IOException {
    if (shouldRefresh()) {
      refresh();
    }
    return accessToken;
  }

  private static boolean closeToExpiring(AccessToken accessToken) {
    return ZonedDateTime.now()
        .isAfter(accessToken.expirationDate().minusMinutes(CLOSE_TO_EXPIRATION_MINUTES));
  }
}
