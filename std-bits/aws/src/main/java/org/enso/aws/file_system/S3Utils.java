package org.enso.aws.file_system;

import com.amazonaws.SdkClientException;
import java.io.IOException;
import java.util.Optional;
import java.util.function.BiFunction;
import org.enso.base.polyglot.EnsoExceptionWrapper;
import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;
import software.amazon.awssdk.services.s3.model.*;

public class S3Utils {
  private S3Utils() {}

  /**
   * Executes the given action and handles any exceptions that may occur during S3 operations.
   *
   * @param action the action to execute, which may throw an exception
   * @return the result of the action if it succeeds
   * @throws RuntimeException if any exception occurs during the execution of the action
   */
  public static Value handleExceptions(
      BiFunction<String, String, Value> action, String bucket, String key) {
    try {
      return action.apply(bucket, key);
    } catch (Exception exception) {
      var ensoAtom =
          Optional.ofNullable(wrapS3Errors(bucket, key, exception))
              .or(() -> Optional.ofNullable(wrapSDKErrors(exception)))
              .or(() -> EnsoExceptionWrapper.wrapCommonExceptions(exception));
      if (ensoAtom.isEmpty()) {
        throw new RuntimeException(exception);
      }
      return EnsoMeta.asDataflowError(ensoAtom.get());
    }
  }

  private static Value wrapSDKErrors(Exception exception) {
    if (exception instanceof SdkClientException sdkClientException) {
      return EnsoMeta.makeInstance(
          "Standard.AWS.Errors", "AWS_SDK_Error", "Error", sdkClientException.getMessage());
    }
    return null;
  }

  private static Value wrapS3Errors(String bucket, String key, Exception exception) {
    return switch (exception) {
      case NoSuchBucketException _ ->
          EnsoMeta.makeInstance("Standard.AWS.Errors", "S3_Bucket_Not_Found", "Error", bucket);
      case NoSuchKeyException _ ->
          EnsoMeta.makeInstance("Standard.AWS.Errors", "S3_Key_Not_Found", "Error", bucket, key);
      case S3Exception s3Exception -> {
        var details = s3Exception.awsErrorDetails();
        var code = details == null ? null : details.errorCode();
        yield EnsoMeta.makeInstance(
            "Standard.AWS.Errors", "S3_Error", "Error", s3Exception.getMessage(), code);
      }
      case IOException ioException ->
          EnsoMeta.makeInstance(
              "Standard.AWS.Errors",
              "S3_Error",
              "Error",
              "An IO error has occurred: " + ioException.getMessage(),
              "s3://" + bucket + "/" + key);
      default -> null;
    };
  }

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

  public static ListObjectVersionsRequest list_object_versions_request(String bucket, String key) {
    return ListObjectVersionsRequest.builder().bucket(bucket).prefix(key).build();
  }
}
