package org.enso.google;

import com.google.auth.oauth2.GoogleCredentials;

class CredentialsHelper {
  /**
   * Materializes wrapped credentials into the proper object.
   *
   * <p>This value should not leak to Enso as it can give the user access to the access token.
   */
  static GoogleCredentials materialize(WrappedGoogleCredentials wrappedGoogleCredentials) {
    return switch (wrappedGoogleCredentials) {
      case WrappedGoogleCredentials.LocalFileCredentials(var inner) -> inner;
      case WrappedGoogleCredentials.SecretCredentials secretCredentials ->
          GoogleOAuthHelper.createCredential(secretCredentials.reference());
    };
  }
}
