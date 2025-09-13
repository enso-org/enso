package org.enso.aws.file_system;

import software.amazon.awssdk.services.s3.model.CopyObjectRequest;
import software.amazon.awssdk.services.s3.model.DeleteObjectRequest;
import software.amazon.awssdk.services.s3.model.GetObjectRequest;
import software.amazon.awssdk.services.s3.model.HeadBucketRequest;
import software.amazon.awssdk.services.s3.model.HeadObjectRequest;
import software.amazon.awssdk.services.s3.model.ListObjectsV2Request;
import software.amazon.awssdk.services.s3.model.PutObjectRequest;

public class S3Utils {
  private S3Utils() {}

  public static DeleteObjectRequest delete_object_request(String bucket, String key) {
    return DeleteObjectRequest.builder().bucket(bucket).key(key).build();
  }

  public static ListObjectsV2Request list_objects_request(
      String bucket, Integer maxKeys, String delimiter, String prefix) {
    return ListObjectsV2Request.builder()
        .bucket(bucket)
        .maxKeys(maxKeys)
        .delimiter(delimiter)
        .prefix(prefix)
        .build();
  }

  public static HeadBucketRequest head_bucket_request(String bucket) {
    return HeadBucketRequest.builder().bucket(bucket).build();
  }

  public static HeadObjectRequest head_object_request(String bucket, String key) {
    return HeadObjectRequest.builder().bucket(bucket).key(key).build();
  }

  public static GetObjectRequest get_object_request(String bucket, String key) {
    return GetObjectRequest.builder().bucket(bucket).key(key).build();
  }

  public static CopyObjectRequest copy_object_request(
      String destinationBucket, String destinationKey, String sourceBucket, String sourceKey) {
    return CopyObjectRequest.builder()
        .destinationBucket(destinationBucket)
        .destinationKey(destinationKey)
        .sourceBucket(sourceBucket)
        .sourceKey(sourceKey)
        .build();
  }

  public static PutObjectRequest put_object_request(String bucket, String key) {
    return PutObjectRequest.builder().bucket(bucket).key(key).build();
  }
}
