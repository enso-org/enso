package org.enso.aws;

import java.util.function.Consumer;
import software.amazon.awssdk.awscore.exception.AwsServiceException;
import software.amazon.awssdk.core.ResponseInputStream;
import software.amazon.awssdk.core.exception.SdkClientException;
import software.amazon.awssdk.core.sync.RequestBody;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.S3ClientBuilder;
import software.amazon.awssdk.services.s3.model.*;

public class S3ClientWrapper implements AutoCloseable {
  private S3Client client;

  private S3ClientWrapper(S3Client client) {
    this.client = client;
  }

  public static S3ClientWrapper from(S3ClientBuilder builder) {
    return new S3ClientWrapper(builder.build());
  }

  public ListBucketsResponse listBuckets()
      throws AwsServiceException, SdkClientException, S3Exception {
    return this.client.listBuckets();
  }

  public ResponseInputStream<GetObjectResponse> getObject(GetObjectRequest getObjectRequest)
      throws NoSuchKeyException,
          InvalidObjectStateException,
          AwsServiceException,
          SdkClientException,
          S3Exception {
    return this.client.getObject(getObjectRequest);
  }

  public HeadBucketResponse headBucket(HeadBucketRequest headBucketRequest)
      throws NoSuchBucketException, AwsServiceException, SdkClientException, S3Exception {
    return this.client.headBucket(headBucketRequest);
  }

  public HeadBucketResponse headBucket(Consumer<HeadBucketRequest.Builder> headBucketRequest)
      throws NoSuchBucketException, AwsServiceException, SdkClientException, S3Exception {
    return this.client.headBucket(headBucketRequest);
  }

  public GetBucketLocationResponse getBucketLocation(
      Consumer<GetBucketLocationRequest.Builder> getBucketLocationRequest)
      throws AwsServiceException, SdkClientException, S3Exception {
    return this.client.getBucketLocation(getBucketLocationRequest);
  }

  public HeadObjectResponse headObject(HeadObjectRequest headObjectRequest)
      throws NoSuchKeyException, AwsServiceException, SdkClientException, S3Exception {
    return this.client.headObject(headObjectRequest);
  }

  public ListObjectsV2Response listObjectsV2(ListObjectsV2Request listObjectsV2Request)
      throws NoSuchBucketException, AwsServiceException, SdkClientException, S3Exception {
    return this.client.listObjectsV2(listObjectsV2Request);
  }

  public PutObjectResponse putObject(PutObjectRequest putObjectRequest, RequestBody requestBody)
      throws AwsServiceException, SdkClientException, S3Exception {
    return this.client.putObject(putObjectRequest, requestBody);
  }

  public DeleteObjectResponse deleteObject(DeleteObjectRequest deleteObjectRequest)
      throws AwsServiceException, SdkClientException, S3Exception {
    return this.client.deleteObject(deleteObjectRequest);
  }

  public CopyObjectResponse copyObject(CopyObjectRequest copyObjectRequest)
      throws ObjectNotInActiveTierErrorException,
          AwsServiceException,
          SdkClientException,
          S3Exception {
    return this.client.copyObject(copyObjectRequest);
  }

  @Override
  public void close() throws Exception {
    this.client.close();
  }
}
