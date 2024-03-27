package org.enso.aws;

import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.regions.providers.AwsRegionProvider;
import software.amazon.awssdk.regions.providers.AwsRegionProviderChain;
import software.amazon.awssdk.regions.providers.DefaultAwsRegionProviderChain;

/** Implements the resolution of AWS Region for Enso AWS_Region.Default and Default_With_Profile. */
public class DefaultRegionProvider extends AwsRegionProviderChain {
  public DefaultRegionProvider(String profileName, Region fallback) {
    super(makeAwsDefault(profileName), new FallbackProvider(fallback));
  }

  private static DefaultAwsRegionProviderChain makeAwsDefault(String profileName) {
    if (profileName == null) {
      return new DefaultAwsRegionProviderChain();
    } else {
      return DefaultAwsRegionProviderChain.builder().profileName(profileName).build();
    }
  }

  private static class FallbackProvider implements AwsRegionProvider {
    private final Region region;

    private FallbackProvider(Region region) {
      this.region = region;
    }

    @Override
    public Region getRegion() {
      return region;
    }
  }
}
