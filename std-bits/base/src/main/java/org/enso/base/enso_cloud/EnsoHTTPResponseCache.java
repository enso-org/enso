package org.enso.base.enso_cloud;

import java.io.IOException;
import java.io.InputStream;
import java.net.http.HttpHeaders;
import java.util.Optional;
import org.enso.base.cache.LRUCache;
import org.enso.base.cache.ResponseTooLargeException;

/**
 * EnsoHTTPResponseCache is a cache for EnsoHttpResponse values that respects the cache control HTTP
 * headers received in the original repsonse to a request.
 *
 * <p>It uses LRUCache, so it also puts limits on the size of files that can be requested, and on
 * the total cache size, deleting entries to make space for new ones. All cache files are set to be
 * deleted automatically on JVM exit.
 *
 * <p>Without caching, EnsoHttpResponse contains an InputStream providing the response data. When
 * there is a cache hit, this stream reads from the local file storing the cached data. When there
 * is no cache hit, the InputStream is connected directly to the remote server.
 */
public class EnsoHTTPResponseCache {
  // Public for testing.
  public EnsoHTTPResponseCache() {}

  // 1 year.
  private final int DEFAULT_TTL_SECONDS = 31536000;

  private LRUCache<Metadata> lruCache = new LRUCache<>();

  public EnsoHttpResponse makeRequest(RequestMaker requestMaker)
      throws IOException, InterruptedException, ResponseTooLargeException {
    var itemBuilder = new ItemBuilder(requestMaker);

    LRUCache.CacheResult<Metadata> cacheResult = lruCache.getResult(itemBuilder);

    return requestMaker.reconstructResponseFromCachedStream(
        cacheResult.inputStream(), cacheResult.metadata());
  }

  public class ItemBuilder implements LRUCache.ItemBuilder<Metadata> {
    private final RequestMaker requestMaker;

    ItemBuilder(RequestMaker requestMaker) {
      this.requestMaker = requestMaker;
    }

    @Override
    public String makeCacheKey() {
      return requestMaker.hashKey();
    }

    /** Only HTTP 200 responses are cached; all others are returned uncached. */
    @Override
    public LRUCache.Item<Metadata> buildItem() throws IOException, InterruptedException {
      var response = requestMaker.makeRequest();

      if (response.statusCode() != 200) {
        // Don't cache non-200 repsonses.
        return new LRUCache.Item<>(
            response.body(),
            new Metadata(response.headers(), response.statusCode()),
            Optional.empty(),
            Optional.empty());
      } else {
        InputStream inputStream = response.body();
        var metadata = new Metadata(response.headers(), response.statusCode());
        var sizeMaybe = getResponseDataSize(response.headers());
        int ttl = calculateTTL(response.headers());
        return new LRUCache.Item<>(inputStream, metadata, sizeMaybe, Optional.of(ttl));
      }
    }
  }

  /** Get the size of the response data, if available. */
  private Optional<Long> getResponseDataSize(HttpHeaders headers) {
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
  private int calculateTTL(HttpHeaders headers) {
    Integer maxAge = getMaxAge(headers);
    if (maxAge == null) {
      return DEFAULT_TTL_SECONDS;
    } else {
      int age = headers.firstValue("age").map(Integer::parseInt).orElse(0);
      return maxAge - age;
    }
  }

  private Integer getMaxAge(HttpHeaders headers) {
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

  /** Public for testing. */
  public LRUCache getLRUCache() {
    return lruCache;
  }

  /** Public for testing. */
  public void setLRUCache(LRUCache<Metadata> lruCache) {
    this.lruCache.clear();
    this.lruCache = lruCache;
  }

  public interface RequestMaker {
    /** Executes the HTTP request and returns the response. */
    EnsoHttpResponse makeRequest() throws IOException, InterruptedException;

    /**
     * Returns a hash key that can be used to uniquely identify this request. This will be used to
     * decide if the `run` method should be executed, or if a cached response will be returned. The
     * hash key should not be reversible.
     */
    String hashKey();

    /**
     * When a cached response is returned, instead of executing `makeRequest`, this method is used
     * to construct the response.
     */
    EnsoHttpResponse reconstructResponseFromCachedStream(
        InputStream inputStream, Metadata metadata);
  }

  public record Metadata(HttpHeaders headers, int statusCode) {}
}
