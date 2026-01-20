package org.enso.base.cache;

import org.enso.base.polyglot.EnsoMeta;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LRUCacheSettings {
  private static final Logger LOGGER = LoggerFactory.getLogger(LRUCacheSettings.class);

  private static final String MAX_FILE_SIZE_ENV_VAR = "ENSO_LIB_HTTP_CACHE_MAX_FILE_SIZE_MB";
  private static final String TOTAL_CACHE_SIZE_ENV_VAR =
      "ENSO_LIB_HTTP_CACHE_MAX_TOTAL_CACHE_LIMIT";

  /**
   * Default value for the largest file size allowed. Should be overridden with the
   * ENSO_LIB_HTTP_CACHE_MAX_FILE_SIZE_MB environment variable.
   */
  private static final long DEFAULT_MAX_FILE_SIZE = 2L * 1024 * 1024 * 1024;

  /**
   * Default value for the percentage of free disk space to use as a limit on the total cache size.
   * Should be overridden with the ENSO_LIB_HTTP_CACHE_MAX_TOTAL_CACHE_LIMIT environment variable.
   */
  private static final double DEFAULT_TOTAL_CACHE_SIZE_FREE_SPACE_PERCENTAGE = 0.2;

  /**
   * Maximum size allowed for a single file. If a file larger than this is requested through this
   * cache, a ResponseTooLargeException is thrown.
   */
  private final long maxFileSize;

  /**
   * Limits the total size of all files in the cache.
   *
   * <p>This value can depend on free disk space, so it is not resolved to a maximum byte count at
   * initialization time, but recalculated during each file cleanup.
   */
  private final TotalCacheLimit.Limit totalCacheLimit;

  public LRUCacheSettings(long maxFileSize, TotalCacheLimit.Limit totalCacheLimit) {
    this.maxFileSize = maxFileSize;
    this.totalCacheLimit = totalCacheLimit;
  }

  public String toString() {
    return "LRUCacheSettings(" + maxFileSize + ", " + totalCacheLimit + ")";
  }

  /** Uses defaults if the vars are not set. */
  public static LRUCacheSettings getDefault() {
    return new LRUCacheSettings(parseMaxFileSizeEnvVar(), parseTotalCacheLimitEnvVar());
  }

  public long getMaxFileSize() {
    return maxFileSize;
  }

  public TotalCacheLimit.Limit getTotalCacheLimit() {
    return totalCacheLimit;
  }

  // Uses the environment variable if set and correctly formatted, otherwise
  // uses a default.
  private static long parseMaxFileSizeEnvVar() {
    var maxFileSizeSpec =
        EnsoMeta.callStaticModuleMethod(
            "Standard.Base.System.Environment", "get", MAX_FILE_SIZE_ENV_VAR);
    if (maxFileSizeSpec.isNull()) {
      return DEFAULT_MAX_FILE_SIZE;
    }
    try {
      double maxFileSizeMegs = Double.parseDouble(maxFileSizeSpec.asString());
      return (long) (maxFileSizeMegs * 1024 * 1024);
    } catch (NumberFormatException e) {
      LOGGER.warn(
          "Unable to parse environment variable "
              + MAX_FILE_SIZE_ENV_VAR
              + ": {}, falling back to default",
          e.getMessage());
      return DEFAULT_MAX_FILE_SIZE;
    }
  }

  // Uses the environment variable if set and correctly formatted, otherwise
  // uses a default.
  private static TotalCacheLimit.Limit parseTotalCacheLimitEnvVar() {
    var totalCacheLimitSpec =
        EnsoMeta.callStaticModuleMethod(
            "Standard.Base.System.Environment", "get", TOTAL_CACHE_SIZE_ENV_VAR);
    if (totalCacheLimitSpec.isNull()) {
      return new TotalCacheLimit.Percentage(DEFAULT_TOTAL_CACHE_SIZE_FREE_SPACE_PERCENTAGE);
    }
    try {
      return TotalCacheLimit.parse(totalCacheLimitSpec.asString());
    } catch (IllegalArgumentException e) {
      LOGGER.warn(
          "Unable to parse environment variable "
              + TOTAL_CACHE_SIZE_ENV_VAR
              + ": {}, falling back to default",
          e.getMessage());
      return new TotalCacheLimit.Percentage(DEFAULT_TOTAL_CACHE_SIZE_FREE_SPACE_PERCENTAGE);
    }
  }
}
