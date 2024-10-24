package org.enso.aws;

import java.net.URI;
import java.net.http.HttpClient;
import java.util.function.Supplier;
import org.enso.base.enso_cloud.ExternalLibrarySecretHelper;
import org.enso.base.enso_cloud.HideableValue;
import software.amazon.awssdk.auth.credentials.AwsBasicCredentials;
import software.amazon.awssdk.auth.credentials.AwsCredentials;
import software.amazon.awssdk.auth.credentials.AwsCredentialsProvider;
import software.amazon.awssdk.auth.credentials.AwsCredentialsProviderChain;
import software.amazon.awssdk.auth.credentials.DefaultCredentialsProvider;
import software.amazon.awssdk.auth.credentials.ProfileCredentialsProvider;
import software.amazon.awssdk.auth.credentials.StaticCredentialsProvider;
import software.amazon.awssdk.core.exception.SdkClientException;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.s3.S3Client;

public class ClientBuilder {
  private static AwsCredential defaultCredentialOverride = null;
  private final AwsCredential awsCredential;
  private final Region awsRegion;

  public ClientBuilder(AwsCredential credential, Region awsRegion) {
    this.awsCredential = credential;
    this.awsRegion = awsRegion;
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
   * Sets an override for what credential should be resolved when `AWS_Credential.Default` is used.
   *
   * <p>It returns the previous override value to allow restoring it if overrides are nested.
   */
  public static AwsCredential setDefaultCredentialOverride(AwsCredential credential) {
    if (credential instanceof AwsCredential.Default) {
      throw new IllegalArgumentException(
          "AWS_Credential.Default is not a valid selection for"
              + " AWS_Credential.set_default_override");
    }

    AwsCredential previous = defaultCredentialOverride;
    defaultCredentialOverride = credential;
    return previous;
  }

  public S3Client buildS3Client() {
    return S3Client.builder()
        .credentialsProvider(unsafeBuildCredentialProvider())
        .region(awsRegion)
        .build();
  }

  /**
   * Signs an AWS request using the provided credentials. AccessKey is returned in header plain
   * text, will reject if credential is AccessKey/SecretKey and AccessKey is secret.
   */
  public HttpClient createSignedClient(
      String regionName, String serviceName, HttpClient baseClient, String bodySHA256) {
    return new SignedHttpClient(
        regionName, serviceName, unsafeBuildCredentialProvider(), baseClient, bodySHA256);
  }

  /**
   * Instantiates an S3Client configured in such a way that it can query buckets regardless of their
   * region.
   *
   * <p>It is used by {@link BucketLocator} to find out the region of buckets.
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
    return unsafeBuildCredentialProvider(awsCredential, this::getDefaultCredentialChain);
  }

  /**
   * The {@code AwsCredentialsProviders} may leak secrets, so it should never be returned to user
   * code.
   */
  private AwsCredentialsProvider unsafeBuildCredentialProvider(
      AwsCredential credential, Supplier<AwsCredentialsProvider> defaultProviderFactory) {
    return switch (credential) {
      case AwsCredential.Default unused -> defaultProviderFactory.get();
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

  private AwsCredentialsProvider getDefaultCredentialChain() {
    AwsCredential override = defaultCredentialOverride;
    if (override != null) {
      return AwsCredentialsProviderChain.builder()
          .credentialsProviders(
              new EnsoOverrideCredentialProvider(override), DefaultCredentialsProvider.create())
          .build();
    } else {
      return DefaultCredentialsProvider.create();
    }
  }

  private class EnsoOverrideCredentialProvider implements AwsCredentialsProvider {
    /**
     * An additional element to the default credentials chain, allowing to override the meaning of
     * `AWS_Credential.Default`, using `AWS_Credential.set_default_override`.
     *
     * <p>It is used mainly for testing.
     */
    private final AwsCredential override;

    private EnsoOverrideCredentialProvider(AwsCredential credential) {
      override = credential;
    }

    @Override
    public AwsCredentials resolveCredentials() {
      Supplier<AwsCredentialsProvider> defaultProviderFactory =
          () -> {
            throw new IllegalArgumentException(
                "AWS_Credential.Default is not a valid selection for AWS_Credential override.");
          };
      return unsafeBuildCredentialProvider(override, defaultProviderFactory).resolveCredentials();
    }
  }
}
