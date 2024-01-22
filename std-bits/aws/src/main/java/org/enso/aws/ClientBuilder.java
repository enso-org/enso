package org.enso.aws;

import org.enso.base.enso_cloud.ExternalLibrarySecretHelper;
import org.enso.base.enso_cloud.HideableValue;
import software.amazon.awssdk.auth.credentials.AwsBasicCredentials;
import software.amazon.awssdk.auth.credentials.AwsCredentialsProvider;
import software.amazon.awssdk.auth.credentials.DefaultCredentialsProvider;
import software.amazon.awssdk.auth.credentials.ProfileCredentialsProvider;
import software.amazon.awssdk.auth.credentials.StaticCredentialsProvider;
import software.amazon.awssdk.services.s3.S3Client;

public class ClientBuilder {
  private final AwsCredential awsCredential;

  public ClientBuilder(AwsCredential credential) {
    this.awsCredential = credential;
  }

  public S3Client buildS3Client() {
    return S3Client.builder().credentialsProvider(unsafeBuildCredentialProvider()).build();
  }

  /**
   * The {@code AwsCredentialsProviders} may leak secrets, so it should never be returned to user
   * code.
   */
  private AwsCredentialsProvider unsafeBuildCredentialProvider() {
    return switch (this.awsCredential) {
      case AwsCredential.Default unused -> DefaultCredentialsProvider.create();
      case AwsCredential.Key key -> {
        AwsBasicCredentials credentials =
            AwsBasicCredentials.create(
                unsafeResolveSecrets(key.accessKeyId()),
                unsafeResolveSecrets(key.secretAccessKey()));
        yield StaticCredentialsProvider.create(credentials);
      }
      case AwsCredential.Profile profile -> ProfileCredentialsProvider.create(profile.name());
    };
  }

  /**
   * This function is allowed access to secrets. Extra care should be taken to ensure its result is
   * not leaked.
   */
  private String unsafeResolveSecrets(HideableValue value) {
    return ExternalLibrarySecretHelper.resolveValue(value);
  }
}
