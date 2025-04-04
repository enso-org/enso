package org.enso.google;

import com.google.auth.oauth2.GoogleCredentials;
import org.enso.base.enso_cloud.ExternalLibraryCredentialHelper;

/** A wrapper for various ways we construct Google credentials. */
public sealed interface WrappedGoogleCredentials {
  record SecretCredentials(ExternalLibraryCredentialHelper.CredentialReference reference)
      implements WrappedGoogleCredentials {}

  @SuppressWarnings("deprecation")
  record LocalFileCredentials(GoogleCredentials credential) implements WrappedGoogleCredentials {}
}
