package org.enso.aws;

import java.io.IOException;
import java.util.Optional;
import java.util.function.BiFunction;
import org.enso.base.polyglot.EnsoExceptionWrapper;
import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;
import software.amazon.awssdk.awscore.exception.AwsServiceException;
import software.amazon.awssdk.core.exception.SdkClientException;
import software.amazon.awssdk.services.s3.model.NoSuchBucketException;
import software.amazon.awssdk.services.s3.model.NoSuchKeyException;
import software.amazon.awssdk.services.s3.model.S3Exception;

class AwsExceptionWrapper {
  private AwsExceptionWrapper() {}

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
      return handleS3ClientError(bucket, key, exception);
    }
  }

  /**
   * Handles exceptions that may occur during S3 client operations and converts them into Enso
   * dataflow errors. It checks for specific S3-related exceptions and wraps them into corresponding
   * Enso error atoms. If the exception does not match any known S3-related exceptions, it attempts
   * to wrap it as a common exception. If the exception cannot be wrapped, it rethrows it as a
   * RuntimeException.
   *
   * @param bucket the name of the S3 bucket involved in the operation
   * @param key the key of the S3 object involved in the operation
   * @param exception the exception that occurred during the S3 client operation
   * @return a Value representing the Enso dataflow error corresponding to the exception
   * @throws RuntimeException if the exception cannot be wrapped into an Enso error atom
   */
  public static Value handleS3ClientError(String bucket, String key, Exception exception) {
    var ensoAtom =
        Optional.ofNullable(wrapS3Errors(bucket, key, exception))
            .or(() -> Optional.ofNullable(wrapSDKErrors(exception)))
            .or(() -> EnsoExceptionWrapper.wrapCommonExceptions(exception));
    if (ensoAtom.isEmpty()) {
      throw new RuntimeException(exception);
    }
    return EnsoMeta.asDataflowError(ensoAtom.get());
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
        var code = details == null ? "" : details.errorCode();
        yield EnsoMeta.makeInstance(
            "Standard.AWS.Errors",
            "S3_Error",
            "Error",
            s3Exception.getMessage(),
            code == null ? "" : code);
      }
      case AwsServiceException awsServiceException -> {
        var details = awsServiceException.awsErrorDetails();
        var code = details == null ? "" : details.errorCode();
        yield EnsoMeta.makeInstance(
            "Standard.AWS.Errors",
            "S3_Error",
            "Error",
            "An AWS service error has occurred: " + awsServiceException.getMessage(),
            code);
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
}
