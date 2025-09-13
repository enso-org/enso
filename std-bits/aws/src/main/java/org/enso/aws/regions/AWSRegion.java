package org.enso.aws.regions;

import java.util.List;
import software.amazon.awssdk.regions.Region;

public final class AWSRegion {

  private final Region region;

  private AWSRegion(Region region) {
    this.region = region;
  }

  public String id() {
    return this.region.id();
  }

  public static AWSRegion of(String region) {
    return new AWSRegion(Region.of(region));
  }

  public static AWSRegion from(Region region) {
    return new AWSRegion(region);
  }

  public static List<AWSRegion> all() {
    return Region.regions().stream().map(AWSRegion::new).toList();
  }

  public static AWSRegion fallbackRegion() {
    return new AWSRegion(Region.EU_WEST_1);
  }

  public static Region underlying(AWSRegion region) {
    return region.region;
  }
}
