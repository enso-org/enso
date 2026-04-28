package org.enso.aws;

import java.net.ProxySelector;
import java.net.http.HttpClient;
import java.time.Duration;
import java.util.function.Function;
import java.util.function.Supplier;
import org.enso.aws.regions.AWSRegion;
import org.enso.base.enso_cloud.EnsoHideableValue;
import org.enso.base.enso_cloud.ExternalLibrarySecretHelper;
import software.amazon.awssdk.auth.credentials.AwsBasicCredentials;
import software.amazon.awssdk.auth.credentials.AwsCredentials;
import software.amazon.awssdk.auth.credentials.AwsCredentialsProvider;
import software.amazon.awssdk.auth.credentials.AwsCredentialsProviderChain;
import software.amazon.awssdk.auth.credentials.DefaultCredentialsProvider;
import software.amazon.awssdk.auth.credentials.ProfileCredentialsProvider;
import software.amazon.awssdk.auth.credentials.StaticCredentialsProvider;
import software.amazon.awssdk.core.exception.SdkClientException;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.presigner.S3Presigner;
import software.amazon.awssdk.services.ses.SesClient;

public class ClientBuilder {
  private static AwsCredential defaultCredentialOverride = null;
  private final AwsCredential awsCredential;
  private final AWSRegion awsRegion;

  public ClientBuilder(AwsCredential credential, AWSRegion awsRegion) {
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

  S3Client buildS3Client() {
    return S3Client.builder()
        .credentialsProvider(unsafeBuildCredentialProvider())
        .region(AWSRegion.underlying(awsRegion))
        .build();
  }

  S3Presigner buildS3Presigner() {
    return S3Presigner.builder()
        .credentialsProvider(unsafeBuildCredentialProvider())
        .region(AWSRegion.underlying(awsRegion))
        .build();
  }

  public SesClient buildSESClient() {
    return SesClient.builder()
        .credentialsProvider(unsafeBuildCredentialProvider())
        .region(AWSRegion.underlying(awsRegion))
        .build();
  }

  /**
   * Builds an HttpClient that will sign requests and payloads using the AWSv4 Signature algorithm.
   */
  public HttpClient createSignedClient(String regionName, String serviceName, String bodySHA256) {
    var baseClient =
        HttpClient.newBuilder()
            .connectTimeout(Duration.ofSeconds(30))
            .followRedirects(HttpClient.Redirect.ALWAYS)
            .proxy(ProxySelector.getDefault())
            .version(HttpClient.Version.HTTP_2)
            .build();

    return new SignedHttpClient(
        regionName, serviceName, unsafeBuildCredentialProvider(), baseClient, bodySHA256);
  }

  /**
   * Gets a Function for hashing a byte[] to a String
   *
   * @return Hashing Function
   */
  public static Function<byte[], String> getSHA256Function() {
    return SignedHttpClient::getSHA256;
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
  private String unsafeResolveSecrets(EnsoHideableValue value) {
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
