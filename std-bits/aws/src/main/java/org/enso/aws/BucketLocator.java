package org.enso.aws;

import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.s3.model.BucketLocationConstraint;

import java.util.HashMap;

/**
 * A utility class for locating the region of an S3 bucket.
 * <p>
 * We cache the region to avoid fetching it all the time.
 * Technically the region may change if a bucket is deleted and re-created in another region,
 * but that seems like a very unlikely scenario, so requiring a restart of the engine in such a case seems OK.
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

  private static Region locateBucket(String bucketName, AwsCredential associatedCredential) {
    DefaultRegionProvider defaultRegionProvider = new DefaultRegionProvider(null, Region.US_EAST_1);
    var clientBuilder = new ClientBuilder(associatedCredential, defaultRegionProvider.getRegion());
    try (var client = clientBuilder.buildS3Client()) {
      var response = client.headBucket(b -> b.bucket(bucketName));
      var regionId = response.sdkHttpResponse().firstMatchingHeader("x-amz-bucket-region");
      return regionId.map(Region::of).orElse(null);
    }
  }
}
