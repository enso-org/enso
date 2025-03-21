package org.enso.google;

import com.google.auth.oauth2.AccessToken;
import com.google.auth.oauth2.GoogleCredentials;
import com.google.auth.oauth2.UserCredentials;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import org.enso.base.enso_cloud.ExternalLibrarySecretHelper;
import org.enso.base.enso_cloud.HideableValue;

public class GoogleOAuthSecretReader {
  public static GoogleCredentials createCredentialFromSecretValue(HideableValue secretValue) {
    String payload = ExternalLibrarySecretHelper.resolveValue(secretValue);
    ByteArrayInputStream stream = new ByteArrayInputStream(payload.getBytes());
    try {
      var loadedCredentials = UserCredentials.fromStream(stream);
      return new CloudRenewableGoogleCredentials(loadedCredentials);
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  private static class CloudRenewableGoogleCredentials extends GoogleCredentials {
    private final UserCredentials underlying;
    private AccessToken token = null;

    public CloudRenewableGoogleCredentials(UserCredentials underlying) {
      super();
      this.underlying = underlying;
    }

    @Override
    public void refresh() throws IOException {
      System.err.println("Refreshing credentials: here we could query the Enso Cloud instead");
      token = underlying.refreshAccessToken();
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
