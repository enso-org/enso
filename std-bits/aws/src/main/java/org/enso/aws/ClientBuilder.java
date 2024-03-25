package org.enso.aws;

import org.enso.base.enso_cloud.ExternalLibrarySecretHelper;
import org.enso.base.enso_cloud.HideableValue;
import software.amazon.awssdk.auth.credentials.AwsBasicCredentials;
import software.amazon.awssdk.auth.credentials.AwsCredentialsProvider;
import software.amazon.awssdk.auth.credentials.DefaultCredentialsProvider;
import software.amazon.awssdk.auth.credentials.ProfileCredentialsProvider;
import software.amazon.awssdk.auth.credentials.StaticCredentialsProvider;
import software.amazon.awssdk.core.exception.SdkClientException;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.s3.S3Client;

import java.net.URI;

public class ClientBuilder {
  private final AwsCredential awsCredential;
  private final Region awsRegion;

  public ClientBuilder(AwsCredential credential, Region awsRegion) {
    this.awsCredential = credential;
    this.awsRegion = awsRegion;
  }

  public S3Client buildS3Client() {
    return S3Client.builder()
        .credentialsProvider(unsafeBuildCredentialProvider())
        .region(awsRegion)
        .build();
  }

  /** Instantiates an S3Client configured in such a way that it can query buckets regardless of their region.
   * <p>
   * It is used by {@link BucketLocator} to find out the region of buckets.
   */
  S3Client buildGlobalS3Client() {
    return S3Client.builder()
        .credentialsProvider(unsafeBuildCredentialProvider())
        .region(Region.US_EAST_1)
        .endpointOverride(URI.create("https://s3.us-east-1.amazonaws.com"))
        .build();
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

  /** Checks if the default credential is available. */
  public static boolean isDefaultCredentialAvailable() {
    try (var provider = DefaultCredentialsProvider.create()) {
      provider.resolveCredentials();
      return true;
    } catch (SdkClientException e) {
      return false;
    }
  }

  /**
   * This function is allowed access to secrets. Extra care should be taken to ensure its result is
   * not leaked.
   */
  private String unsafeResolveSecrets(HideableValue value) {
    return ExternalLibrarySecretHelper.resolveValue(value);
  }
}
