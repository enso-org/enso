package org.enso.aws;

import org.enso.aws.file_system.S3Utils;
import org.graalvm.polyglot.Value;
import software.amazon.awssdk.awscore.exception.AwsServiceException;
import software.amazon.awssdk.core.ResponseInputStream;
import software.amazon.awssdk.core.exception.SdkClientException;
import software.amazon.awssdk.core.sync.RequestBody;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.S3ClientBuilder;
import software.amazon.awssdk.services.s3.model.*;

public class S3ClientWrapper implements AutoCloseable {
  S3Client client;

  private S3ClientWrapper(S3Client client) {
    this.client = client;
  }

  public static S3ClientWrapper from(S3ClientBuilder builder) {
    return new S3ClientWrapper(builder.build());
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

  HeadBucketResponse headBucketInternal(String bucket) throws NoSuchBucketException, AwsServiceException, SdkClientException {
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

  public ResponseInputStream<GetObjectResponse> getObject(GetObjectRequest getObjectRequest)
      throws NoSuchKeyException,
          InvalidObjectStateException,
          AwsServiceException,
          SdkClientException,
          S3Exception {
    return client.getObject(getObjectRequest);
  }

  public ListObjectsV2Response listObjectsV2(ListObjectsV2Request listObjectsV2Request)
      throws NoSuchBucketException, AwsServiceException, SdkClientException, S3Exception {
    return client.listObjectsV2(listObjectsV2Request);
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
