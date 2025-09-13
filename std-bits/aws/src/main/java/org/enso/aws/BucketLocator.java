package org.enso.aws;

import java.util.HashMap;
import java.util.Optional;
import org.enso.aws.regions.AWSRegion;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import software.amazon.awssdk.http.SdkHttpResponse;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.s3.model.BucketLocationConstraint;
import software.amazon.awssdk.services.s3.model.HeadBucketResponse;
import software.amazon.awssdk.services.s3.model.S3Exception;

/**
 * A utility class for locating the region of an S3 bucket.
 *
 * <p>We cache the region to avoid fetching it all the time. Technically the region may change if a
 * bucket is deleted and re-created in another region, but that seems like a very unlikely scenario,
 * and usually the new bucket with the same name cannot be created immediately, so requiring a
 * restart of the engine in such a case seems OK.
 */
public class BucketLocator {
  private static final HashMap<String, AWSRegion> cache = new HashMap<>();
  private static final Logger LOGGER = LoggerFactory.getLogger(BucketLocator.class);

  public static void flushCache() {
    cache.clear();
  }

  public static AWSRegion getBucketRegion(String bucketName, AwsCredential associatedCredential) {
    if (cache.containsKey(bucketName)) {
      return cache.get(bucketName);
    }

    AWSRegion found = locateBucket(bucketName, associatedCredential);
    cache.put(bucketName, found);
    return found;
  }

  private static AWSRegion locateBucket(String bucketName, AwsCredential associatedCredential) {
    AWSRegion region = locateBucketUsingHead(bucketName, associatedCredential);
    if (region != null) {
      return region;
    }

    return locateBucketLegacy(bucketName, associatedCredential);
  }

  /**
   * The AWS docs recommend to use HeadBucket operation to get the region.
   *
   * <p>However, in practice we noticed it usually fails if the region the client is using is wrong.
   * But the error response, contains the true bucket region which we can extract.
   */
  private static AWSRegion locateBucketUsingHead(
      String bucketName, AwsCredential associatedCredential) {
    var clientBuilder = new ClientBuilder(associatedCredential, null);
    try (var client = clientBuilder.buildGlobalS3Client()) {
      HeadBucketResponse response = client.headBucket(builder -> builder.bucket(bucketName));
      return findRegionInResponse(response.sdkHttpResponse());
    } catch (S3Exception error) {
      var details = error.awsErrorDetails();
      if (details == null) {
        LOGGER.trace("Failed to locate a bucket (missing details in error response).", error);
        return null;
      }

      // We can extract the region from the error response as well.
      return findRegionInResponse(details.sdkHttpResponse());
    } catch (Exception e) {
      LOGGER.trace("Failed to locate a bucket using HeadBucket.", e);
      return null;
    }
  }

  private static AWSRegion findRegionInResponse(SdkHttpResponse response) {
    Optional<String> regionId = response.firstMatchingHeader("x-amz-bucket-region");
    return regionId.map(AWSRegion::of).orElse(null);
  }

  /**
   * If the new way of getting the region does not work, we use the legacy method. It may not be
   * able to recognize all regions, so it is only used as a fallback.
   */
  private static AWSRegion locateBucketLegacy(
      String bucketName, AwsCredential associatedCredential) {
    var clientBuilder = new ClientBuilder(associatedCredential, null);
    try (var client = clientBuilder.buildGlobalS3Client()) {
      BucketLocationConstraint locationConstraint =
          client.getBucketLocation(builder -> builder.bucket(bucketName)).locationConstraint();
      if (locationConstraint == null) {
        // Weird edge case: documentation says that buckets in region us-east-1 return null
        return AWSRegion.from(Region.US_EAST_1);
      }

      if (locationConstraint == BucketLocationConstraint.UNKNOWN_TO_SDK_VERSION) {
        LOGGER.trace("AWS returned an unknown location constraint.");
        return null;
      }

      var inferredRegion = AWSRegion.of(locationConstraint.toString());
      boolean isKnown = AWSRegion.all().contains(inferredRegion);
      if (isKnown) {
        return inferredRegion;
      } else {
        LOGGER.trace(
            "AWS returned a location constraint that cannot be mapped to a known region: {}",
            locationConstraint);
        return null;
      }
    } catch (Exception e) {
      LOGGER.trace("Failed to locate a bucket (legacy GetBucketLocation).", e);
      return null;
    }
  }
}
