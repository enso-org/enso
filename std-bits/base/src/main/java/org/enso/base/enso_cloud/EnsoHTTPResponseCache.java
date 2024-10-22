package org.enso.base.enso_cloud;

import java.io.IOException;
import java.io.InputStream;
import java.net.http.HttpHeaders;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Optional;
import org.enso.base.cache.ResponseTooLargeException;
import org.enso.base.cache.StreamCache;
import org.enso.base.cache.StreamCache.CacheResult;
import org.enso.base.cache.StreamCache.StreamMaker;

/**
 * EnsoHTTPResponseCache  is a cache for EnsoHttpResponse values that respects the cache control
 * HTTP headers received in the original repsonse to a request.
 *
 * <p>It uses StreamCache, so it also puts limits on the size of files that can
 * be requested, and on the total cache size, deleting entries to make space for
 * new ones. All cache files are set to be deleted automatically on JVM exit.
 *
 * <p>Without caching, EnsoHttpResponse contains an InputStream providing the response data. When
 * there is a cache hit, this stream reads from the local file storing the cached data. When there
 * is no cache hit, the InputStream is connected directly to the remote server.
 */
public class EnsoHTTPResponseCache {
  private static final int DEFAULT_TTL_SECONDS = 31536000;
  private static final long MAX_FILE_SIZE = 10L * 1024 * 1024;
  private static final long MAX_TOTAL_CACHE_SIZE = 10L * 1024 * 1024 * 1024;

  private static final StreamCache<Metadata> streamCache = new StreamCache<>(MAX_FILE_SIZE, MAX_TOTAL_CACHE_SIZE);

  public static EnsoHttpResponse makeRequest(RequestMaker requestMaker) throws IOException, InterruptedException, ResponseTooLargeException {
    StreamMaker<Metadata> streamMaker = new EnsoHTTPResponseCacheThing(requestMaker);

    CacheResult<Metadata> cacheResult = streamCache.getResult(streamMaker);

    return requestMaker.reconstructResponseFromCachedStream(cacheResult.inputStream(), cacheResult.metadata());
  }

  public static class EnsoHTTPResponseCacheThing implements StreamMaker<Metadata> {
    private final RequestMaker requestMaker;

    EnsoHTTPResponseCacheThing(RequestMaker requestMaker) {
      this.requestMaker = requestMaker;
    }

    @Override
    public String makeCacheKey() {
      return requestMaker.hashKey();
    }

    /**
     * <p>Only HTTP 200 responses are cached; all others are returned uncached.
     */
    @Override
    public StreamCache.Thing<Metadata> makeThing() throws IOException, InterruptedException {
      var response = requestMaker.run();

      if (response.statusCode() != 200) {
        // Don't cache non-200 repsonses.
        return new StreamCache.Thing<>(
            response.body(),
            new Metadata(response.headers(), response.statusCode()),
            Optional.empty(),
            Optional.empty());
      } else {
        InputStream inputStream = response.body();
        var metadata = new Metadata(response.headers(), response.statusCode());
        var sizeMaybe = getResponseDataSize(response.headers());
        int ttl = calculateTTL(response.headers());
        return new StreamCache.Thing<>(inputStream, metadata, sizeMaybe, Optional.of(ttl));
      }
    }
  }

  /** Get the size of the response data, if available. */
  private static Optional<Long> getResponseDataSize(HttpHeaders headers) {
    return headers.firstValue("content-length").map(Long::parseLong);
  }

  /**
   * We define the TTL as the amount of time that the response should be considered fresh.
   *
   * <p>Define t0 as the time at which the content was generated on the origin server.
   *
   * <p>Define t1 as the time at which the current request was handled, either by the origin server
   * or an intervening cache.
   *
   * <p>The 'Age' header, if present is (t1 - t0).
   *
   * <p>The 'max-age' value in the 'Cache-Control' header, if present, is the origin server's
   * definition of how long the response should be considered fresh.
   *
   * <p>If 'max-age' and 'Age' are both present, we set TTL = max-age - Age. If only 'max-age' is
   * present, we set TTL = max-age. If neither are present, we use a default.
   */
  private static int calculateTTL(HttpHeaders headers) {
    Integer maxAge = getMaxAge(headers);
    if (maxAge == null) {
      return DEFAULT_TTL_SECONDS;
    } else {
      int age = headers.firstValue("age").map(Integer::parseInt).orElse(0);
      return maxAge - age;
    }
  }

  private static Integer getMaxAge(HttpHeaders headers) {
    var cacheControlMaybe = headers.firstValue("cache-control");
    Integer maxAge = null;
    if (cacheControlMaybe.isPresent()) {
      var cacheControl = cacheControlMaybe.get();
      var cacheControlEntries = cacheControl.split(",");
      for (var entry : cacheControlEntries) {
        if (entry.trim().toLowerCase().startsWith("max-age")) {
          var maxAgeBinding = entry.split("=");
          if (maxAgeBinding.length > 1) {
            maxAge = Integer.valueOf(maxAgeBinding[1]);
          }
          break;
        }
      }
    }
    return maxAge;
  }

  public static void clear() {
    streamCache.clear();
  }

  public static int getNumEntries() {
    return streamCache.getNumEntries();
  }

  public static List<Long> getFileSizesTestOnly() {
    return streamCache.getFileSizesTestOnly();
  }

  public static void setNowOverrideTestOnly(ZonedDateTime nowOverride) {
    streamCache.setNowOverrideTestOnly(nowOverride);
  }

  public static void clearNowOverrideTestOnly() {
    streamCache.clearNowOverrideTestOnly();
  }

  public static void setMaxFileSizeOverrideTestOnly(long maxFileSizeOverrideTestOnly) {
    streamCache.setMaxFileSizeOverrideTestOnly(maxFileSizeOverrideTestOnly);
  }

  public static void clearMaxFileSizeOverrideTestOnly() {
    streamCache.clearMaxFileSizeOverrideTestOnly();
  }

  public static void setMaxTotalCacheSizeOverrideTestOnly(long maxTotalCacheSizeOverrideTestOnly_) {
    streamCache.setMaxTotalCacheSizeOverrideTestOnly(maxTotalCacheSizeOverrideTestOnly_);
  }

  public static void clearMaxTotalCacheSizeOverrideTestOnly() {
    streamCache.clearMaxTotalCacheSizeOverrideTestOnly();
  }

  public interface RequestMaker {
    EnsoHttpResponse run() throws IOException, InterruptedException;

    String hashKey();

    EnsoHttpResponse reconstructResponseFromCachedStream(
        InputStream inputStream, Metadata metadata);
  }

  public record Metadata(HttpHeaders headers, int statusCode) {}
}
