package org.enso.google;

import com.google.api.client.googleapis.auth.oauth2.GoogleCredential;
import org.enso.base.enso_cloud.ExternalLibrarySecretHelper;
import org.enso.base.enso_cloud.HideableValue;

import java.io.ByteArrayInputStream;
import java.io.IOException;

public class GoogleOAuthSecretReader {
  public static GoogleCredential createCredentialFromSecretValue(HideableValue secretValue) {
    String payload = ExternalLibrarySecretHelper.resolveValue(secretValue);
    ByteArrayInputStream stream = new ByteArrayInputStream(payload.getBytes());
    try {
      return GoogleCredential.fromStream(stream);
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }
}
