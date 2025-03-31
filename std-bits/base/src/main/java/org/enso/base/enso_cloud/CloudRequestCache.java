package org.enso.base.enso_cloud;

import org.enso.base.cache.APIRequestCache;
import org.enso.base.cache.ReloadDetector;

/**
 * A cache that can be used to save results of cloud requests to avoid re-fetching them every time.
 */
public final class CloudRequestCache extends APIRequestCache {
  public CloudRequestCache() {
    ReloadDetector.register(this);
  }

  public static final CloudRequestCache INSTANCE = new CloudRequestCache();
}
