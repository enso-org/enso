package org.enso.google;

import com.google.api.gax.core.CredentialsProvider;
import com.google.api.gax.core.FixedCredentialsProvider;

public class GoogleCredentialsProvider {
  private final CredentialsProvider provider;

  private GoogleCredentialsProvider(CredentialsProvider provider) {
    this.provider = provider;
  }

  public static GoogleCredentialsProvider fromGoogleCredentials(
      WrappedGoogleCredentials googleCredentials) {
    return switch (googleCredentials) {
      case WrappedGoogleCredentials.SecretCredentials ref -> null;
      case WrappedGoogleCredentials.LocalFileCredentials localCredentials ->
          new GoogleCredentialsProvider(
              FixedCredentialsProvider.create(localCredentials.credential()));
    };
  }

  public static CredentialsProvider underlying(GoogleCredentialsProvider credentialsProvider) {
    return credentialsProvider.provider;
  }
}
