package org.enso.google;

import com.google.auth.oauth2.GoogleCredentials;
import org.enso.base.enso_cloud.ExternalLibrarySecretHelper;
import org.enso.base.enso_cloud.HideableValue;

import java.io.ByteArrayInputStream;
import java.io.IOException;

public class GoogleOAuthSecretReader {
  public static GoogleCredentials createCredentialFromSecretValue(HideableValue secretValue) {
    String payload = ExternalLibrarySecretHelper.resolveValue(secretValue);
    ByteArrayInputStream stream = new ByteArrayInputStream(payload.getBytes());
    try {
      return GoogleCredentials.fromStream(stream);
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }
}
