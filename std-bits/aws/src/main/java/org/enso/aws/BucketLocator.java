package org.enso.aws;

import software.amazon.awssdk.http.SdkHttpResponse;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.s3.model.HeadBucketResponse;
import software.amazon.awssdk.services.s3.model.S3Exception;

import java.util.HashMap;
import java.util.Optional;
import java.util.logging.Logger;

/**
 * A utility class for locating the region of an S3 bucket.
 * <p>
 * We cache the region to avoid fetching it all the time.
 * Technically the region may change if a bucket is deleted and re-created in another region,
 * but that seems like a very unlikely scenario, and usually the new bucket with the same name cannot be created immediately,
 * so requiring a restart of the engine in such a case seems OK.
 */
public class BucketLocator {
  private static final HashMap<String, Region> cache = new HashMap<>();

  public static void flushCache() {
    cache.clear();
  }

  public static Region getBucketRegion(String bucketName, AwsCredential associatedCredential) {
    if (cache.containsKey(bucketName)) {
      return cache.get(bucketName);
    }

    Region found = locateBucket(bucketName, associatedCredential);
    cache.put(bucketName, found);
    return found;
  }

  /** The AWS docs recommend to use HeadBucket operation to get the region.
   * <p>
   * However, in practice we noticed it usually fails if the region the client is using is wrong.
   * But the error response, contains the true bucket region which we can extract. */
  private static Region locateBucket(String bucketName, AwsCredential associatedCredential) {
    var clientBuilder = new ClientBuilder(associatedCredential, null);
    try (var client = clientBuilder.buildGlobalS3Client()) {
      HeadBucketResponse response = client.headBucket(builder -> builder.bucket(bucketName));
      return findRegionInResponse(response.sdkHttpResponse());
    } catch (S3Exception error) {
      var details = error.awsErrorDetails();
      if (details == null) {
        logger.fine("Failed to locate a bucket (missing details in error response): " + error.getMessage());
        return null;
      }

      // We can extract the region from the error response as well.
      return findRegionInResponse(details.sdkHttpResponse());
    } catch (Exception e) {
      logger.fine("Failed to locate a bucket using HeadBucket: " + e.getMessage());
      return null;
    }
  }

  private static Region findRegionInResponse(SdkHttpResponse response) {
    Optional<String> regionId = response.firstMatchingHeader("x-amz-bucket-region");
    return regionId.map(Region::of).orElse(null);
  }

  private static final Logger logger = Logger.getLogger(BucketLocator.class.getName());
}
