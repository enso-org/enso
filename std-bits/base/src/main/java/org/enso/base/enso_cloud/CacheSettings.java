package org.enso.base.enso_cloud;

import java.time.Duration;

public final class CacheSettings {
  private static Duration fileCacheTTL = Duration.ofSeconds(60);

  public static Duration getFileCacheTTL() {
    return fileCacheTTL;
  }

  public static void setFileCacheTTL(Duration fileCacheTTL) {
    CacheSettings.fileCacheTTL = fileCacheTTL;
  }
}
