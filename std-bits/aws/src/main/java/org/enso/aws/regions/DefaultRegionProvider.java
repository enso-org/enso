package org.enso.aws.regions;

import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.regions.providers.AwsRegionProvider;
import software.amazon.awssdk.regions.providers.AwsRegionProviderChain;
import software.amazon.awssdk.regions.providers.DefaultAwsRegionProviderChain;

/** Implements the resolution of AWS Region for Enso AWS_Region.Default and Default_With_Profile. */
public final class DefaultRegionProvider extends AwsRegionProviderChain {
  private DefaultRegionProvider(String profileName, AWSRegion fallback) {
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
    private final AWSRegion region;

    private FallbackProvider(AWSRegion region) {
      this.region = region;
    }

    @Override
    public Region getRegion() {
      return AWSRegion.underlying(region);
    }
  }

  public static String default_region_for_profile(String profileName, AWSRegion fallback) {
    return new DefaultRegionProvider(profileName, fallback).getRegion().id();
  }
}
