package org.enso.google;

import com.google.auth.oauth2.GoogleCredentials;
import java.io.IOException;
import java.io.InputStream;
import org.enso.base.enso_cloud.ExternalLibraryCredentialHelper;

/** A wrapper for various ways we construct Google credentials. */
public sealed interface WrappedGoogleCredentials {
  record SecretCredentials(ExternalLibraryCredentialHelper.CredentialReference reference)
      implements WrappedGoogleCredentials {}

  @SuppressWarnings("deprecation")
  record LocalFileCredentials(GoogleCredentials credential) implements WrappedGoogleCredentials {
    static LocalFileCredentials fromStream(InputStream stream) throws IOException {
      return new LocalFileCredentials(GoogleCredentials.fromStream(stream));
    }
  }
}
