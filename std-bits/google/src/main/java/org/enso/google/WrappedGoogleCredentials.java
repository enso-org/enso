package org.enso.google;

import com.google.auth.oauth2.GoogleCredentials;
import java.io.FileInputStream;
import java.io.IOException;
import org.enso.base.enso_cloud.ExternalLibraryCredentialHelper;

/** A wrapper for various ways we construct Google credentials. */
public sealed interface WrappedGoogleCredentials {
  record SecretCredentials(ExternalLibraryCredentialHelper.CredentialReference reference)
      implements WrappedGoogleCredentials {}

  record LocalFileCredentials(GoogleCredentials credential) implements WrappedGoogleCredentials {}

  static SecretCredentials fromCredentialReference(
      ExternalLibraryCredentialHelper.CredentialReference reference) {
    return new SecretCredentials(reference);
  }

  static LocalFileCredentials fromFile(String path) throws IOException {
    try (var stream = new FileInputStream(path)) {
      return new LocalFileCredentials(GoogleCredentials.fromStream(stream));
    }
  }
}
