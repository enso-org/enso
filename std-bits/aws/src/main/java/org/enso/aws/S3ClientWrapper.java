package org.enso.aws;

import java.util.ArrayList;
import java.util.List;
import org.enso.aws.file_system.S3Utils;
import org.enso.aws.regions.AWSRegion;
import org.graalvm.polyglot.Value;
import software.amazon.awssdk.awscore.exception.AwsServiceException;
import software.amazon.awssdk.core.ResponseInputStream;
import software.amazon.awssdk.core.exception.SdkClientException;
import software.amazon.awssdk.core.sync.RequestBody;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.model.*;

public class S3ClientWrapper implements AutoCloseable {
  S3Client client;

  S3ClientWrapper(S3Client client) {
    this.client = client;
  }

  public static S3ClientWrapper forCredential(AwsCredential credential, AWSRegion region) {
    var builder = new ClientBuilder(credential, region);
    return new S3ClientWrapper(builder.buildS3Client());
  }

  public static S3ClientWrapper forBucket(
          AwsCredential credential, String bucketName, AWSRegion defaultRegion) {
    var bucketRegion = BucketLocator.getBucketRegion(bucketName, credential);
    if (bucketRegion == null) {
      bucketRegion = defaultRegion;
    }
    return forCredential(credential, bucketRegion);
  }

  public Value listBuckets() {
    try {
      var response = client.listBuckets();
      var array = response.buckets().stream().map(Bucket::name).toArray(String[]::new);
      return Value.asValue(array);
    } catch (Exception exception) {
      return S3Utils.handleS3ClientError("", "", exception);
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
      return S3Utils.handleS3ClientError(bucket, key, exception);
    }
  }

  public Value headBucket(String bucket) {
    try {
      var response = headBucketInternal(bucket);
      return Value.asValue(response);
    } catch (Exception exception) {
      return S3Utils.handleS3ClientError(bucket, "", exception);
    }
  }

  HeadBucketResponse headBucketInternal(String bucket)
      throws NoSuchBucketException, AwsServiceException, SdkClientException {
    var request = HeadBucketRequest.builder().bucket(bucket).build();
    return client.headBucket(request);
  }

  public Value headObject(String bucket, String key) {
    try {
      var request = HeadObjectRequest.builder().bucket(bucket).key(key).build();
      var response = client.headObject(request);
      return Value.asValue(response);
    } catch (Exception exception) {
      return S3Utils.handleS3ClientError(bucket, key, exception);
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
      return S3Utils.handleS3ClientError(bucket, prefix, exception);
    }
  }

  public ResponseInputStream<GetObjectResponse> getObject(GetObjectRequest getObjectRequest)
      throws NoSuchKeyException,
          InvalidObjectStateException,
          AwsServiceException,
          SdkClientException,
          S3Exception {
    return client.getObject(getObjectRequest);
  }

  public PutObjectResponse putObject(PutObjectRequest putObjectRequest, RequestBody requestBody)
      throws AwsServiceException, SdkClientException, S3Exception {
    return client.putObject(putObjectRequest, requestBody);
  }

  public DeleteObjectResponse deleteObject(DeleteObjectRequest deleteObjectRequest)
      throws AwsServiceException, SdkClientException, S3Exception {
    return client.deleteObject(deleteObjectRequest);
  }

  public CopyObjectResponse copyObject(CopyObjectRequest copyObjectRequest)
      throws ObjectNotInActiveTierErrorException,
          AwsServiceException,
          SdkClientException,
          S3Exception {
    return client.copyObject(copyObjectRequest);
  }

  @Override
  public void close() throws Exception {
    client.close();
  }
}
