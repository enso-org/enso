package org.enso.aws;

import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import org.enso.aws.regions.AWSRegion;
import org.graalvm.polyglot.Value;
import software.amazon.awssdk.awscore.exception.AwsServiceException;
import software.amazon.awssdk.core.exception.SdkClientException;
import software.amazon.awssdk.core.sync.RequestBody;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.model.*;
import software.amazon.awssdk.services.s3.presigner.model.GetObjectPresignRequest;

public class S3ClientWrapper implements AutoCloseable {
  final ClientBuilder clientBuilder;
  final S3Client client;

  S3ClientWrapper(ClientBuilder clientBuilder) {
    this.clientBuilder = clientBuilder;
    this.client = clientBuilder.buildS3Client();
  }

  static S3ClientWrapper forCredentialInternal(AwsCredential credential, AWSRegion region) {
    var builder = new ClientBuilder(credential, region);
    return new S3ClientWrapper(builder);
  }

  public static Value forCredential(AwsCredential credential, AWSRegion region) {
    try {
      return Value.asValue(forCredentialInternal(credential, region));
    } catch (Exception exception) {
      return AwsExceptionWrapper.handleS3ClientError("", "", exception);
    }
  }

  public static Value forBucket(
      AwsCredential credential, String bucketName, AWSRegion defaultRegion) {
    try {
      var bucketRegion = BucketLocator.getBucketRegion(bucketName, credential);
      if (bucketRegion == null) {
        bucketRegion = defaultRegion;
      }
      return Value.asValue(forCredentialInternal(credential, bucketRegion));
    } catch (Exception exception) {
      return AwsExceptionWrapper.handleS3ClientError(bucketName, "", exception);
    }
  }

  public Value listBuckets() {
    try {
      var response = client.listBuckets();
      var array = response.buckets().stream().map(Bucket::name).toArray(String[]::new);
      return Value.asValue(array);
    } catch (Exception exception) {
      return AwsExceptionWrapper.handleS3ClientError("", "", exception);
    }
  }

  public Value listObjectVersions(String bucket, String key) {
    try {
      var request = ListObjectVersionsRequest.builder().bucket(bucket).prefix(key).build();
      var response = client.listObjectVersions(request);

      if (!response.hasVersions()) {
        throw new IllegalArgumentException("No versions found for s3://" + bucket + "/" + key);
      }

      var array =
          response.versions().stream()
              .map(v -> "null".equals(v.versionId()) ? null : v.versionId())
              .toArray(String[]::new);
      return Value.asValue(array);
    } catch (Exception exception) {
      return AwsExceptionWrapper.handleS3ClientError(bucket, key, exception);
    }
  }

  public Value headBucket(String bucket) {
    try {
      var response = headBucketInternal(bucket);
      return Value.asValue(response);
    } catch (Exception exception) {
      return AwsExceptionWrapper.handleS3ClientError(bucket, "", exception);
    }
  }

  HeadBucketResponse headBucketInternal(String bucket)
      throws AwsServiceException, SdkClientException {
    var request = HeadBucketRequest.builder().bucket(bucket).build();
    return client.headBucket(request);
  }

  public Value headObject(String bucket, String key, String versionId) {
    try {
      var request = HeadObjectRequest.builder().bucket(bucket).key(key);
      if (versionId != null && !versionId.equals("null") && !versionId.isEmpty()) {
        request = request.versionId(versionId);
      }
      var response = client.headObject(request.build());
      return Value.asValue(response);
    } catch (Exception exception) {
      return AwsExceptionWrapper.handleS3ClientError(bucket, key, exception);
    }
  }

  public record ReadBucketResult(List<String> keys, List<String> prefixes, boolean finished) {}

  public Value readBucket(String bucket, String prefix, String delimiter, int maxCounts) {
    try {
      int perRequest = Math.min(1000, Math.max(0, maxCounts));
      if (perRequest == 0) {
        return Value.asValue(new String[0]);
      }

      var request =
          ListObjectsV2Request.builder()
              .bucket(bucket)
              .prefix(prefix)
              .delimiter(delimiter)
              .maxKeys(maxCounts)
              .build();

      List<String> prefixes = null;
      var keys = new ArrayList<String>();
      boolean finished = false;

      while (!finished && keys.size() < maxCounts) {
        var response = client.listObjectsV2(request);

        if (prefixes == null) {
          // Note the AWS API does not limit the count of common prefixes.
          prefixes = response.commonPrefixes().stream().map(CommonPrefix::prefix).toList();
        }

        keys.addAll(response.contents().stream().map(S3Object::key).toList());
        finished = !response.isTruncated();

        if (!finished) {
          perRequest = Math.min(1000, Math.max(0, maxCounts - keys.size()));
          request =
              request.toBuilder()
                  .continuationToken(response.nextContinuationToken())
                  .maxKeys(perRequest)
                  .build();
        }
      }

      return Value.asValue(new ReadBucketResult(keys, prefixes, finished));
    } catch (Exception exception) {
      return AwsExceptionWrapper.handleS3ClientError(bucket, prefix, exception);
    }
  }

  public Value getObject(String bucket, String key, String versionId) {
    try {
      var request = GetObjectRequest.builder().bucket(bucket).key(key);
      if (versionId != null && !versionId.equals("null") && !versionId.isEmpty()) {
        request = request.versionId(versionId);
      }

      var response = client.getObject(request.build());
      return Value.asValue(response);
    } catch (Exception exception) {
      return AwsExceptionWrapper.handleS3ClientError(bucket, key, exception);
    }
  }

  public Value putObjectFromText(String bucket, String key, String content) {
    try {
      var body = RequestBody.fromString(content);
      var response = innerPutObject(bucket, key, body);
      return Value.asValue(response);
    } catch (Exception exception) {
      return AwsExceptionWrapper.handleS3ClientError(bucket, key, exception);
    }
  }

  public Value putObjectFromFile(String bucket, String key, String filePath) {
    try {
      var body = RequestBody.fromFile(Path.of(filePath));
      var response = innerPutObject(bucket, key, body);
      return Value.asValue(response);
    } catch (Exception exception) {
      return AwsExceptionWrapper.handleS3ClientError(bucket, key, exception);
    }
  }

  private PutObjectResponse innerPutObject(String bucket, String key, RequestBody body) {
    var request = PutObjectRequest.builder().bucket(bucket).key(key);
    if (body.optionalContentLength().isPresent()) {
      request = request.contentLength(body.optionalContentLength().get());
    }
    if (null != body.contentType() && !body.contentType().isEmpty()) {
      request = request.contentType(body.contentType());
    }
    return client.putObject(request.build(), body);
  }

  public Value deleteObject(String bucket, String key) {
    try {
      var request = DeleteObjectRequest.builder().bucket(bucket).key(key).build();
      var response = client.deleteObject(request);
      return Value.asValue(response);
    } catch (Exception exception) {
      return AwsExceptionWrapper.handleS3ClientError(bucket, key, exception);
    }
  }

  public Value copyObject(
      String destinationBucket, String destinationKey, String sourceBucket, String sourceKey) {
    try {
      var request =
          CopyObjectRequest.builder()
              .destinationBucket(destinationBucket)
              .destinationKey(destinationKey)
              .sourceBucket(sourceBucket)
              .sourceKey(sourceKey)
              .build();
      var response = client.copyObject(request);
      return Value.asValue(response);
    } catch (Exception exception) {
      return AwsExceptionWrapper.handleS3ClientError(sourceBucket, sourceKey, exception);
    }
  }

  public Value signedUri(String bucket, String key, String versionId, int expirationSeconds) {
    try {
      var request = GetObjectRequest.builder().bucket(bucket).key(key);
      if (versionId != null && !versionId.equals("null") && !versionId.isEmpty()) {
        request = request.versionId(versionId);
      }

      var presignRequest =
          GetObjectPresignRequest.builder()
              .signatureDuration(Duration.ofSeconds(expirationSeconds))
              .getObjectRequest(request.build());

      try (var presigner = clientBuilder.buildS3Presigner()) {
        var presignResponse = presigner.presignGetObject(presignRequest.build());
        return Value.asValue(presignResponse.url().toExternalForm());
      }
    } catch (Exception exception) {
      return AwsExceptionWrapper.handleS3ClientError(bucket, key, exception);
    }
  }

  @Override
  public void close() throws Exception {
    client.close();
  }
}
