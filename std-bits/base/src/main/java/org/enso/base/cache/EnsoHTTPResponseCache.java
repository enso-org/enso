package org.enso.base.cache;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.http.HttpHeaders;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HexFormat;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.function.Predicate;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import org.apache.commons.io.IOUtils;
import org.enso.base.cache.StreamCache.StreamMaker;
import org.enso.base.net.URIWithSecrets;
import org.graalvm.collections.Pair;
import org.enso.base.enso_cloud.EnsoHttpResponse;

public class EnsoHTTPResponseCache {
  private static StreamCache<Metadata> streamCache = new StreamCache<>();

  private static final int DEFAULT_TTL_SECONDS = 31536000;

  public static EnsoHttpResponse makeRequest(RequestMaker requestMaker) {
      StreamMaker<Metadata> streamMaker = new EnsoHTTPResponseCacheThing(requestMaker);

      CacheResult<Metadata> cacheResult = streamCache.getResult(streamMaker);

      return requestMaker.reconstructResponseFromCachedStream(inputStream, cacheResult.metadata());
  }

  public static class EnsoHTTPResponseCacheThing implements StreamMaker {
      private RequestMaker requestMaker;

      EnsoHTTPResponseCacheThing(RequestMaker requestMaker) {
          this.requestMaker = requestMaker;
      }

      String makeCacheKey() {
          return requestMaker.hashKey();
      }

      Thing makeThing() {
          EnsoHTTPResponse response = requestMaker.run();

          if (response.statusCode() != 200) {
            // Don't cache non-200 repsonses.
            return new Thing(
                response.body(),
                new Metadata(response.headers(), response.statusCode()),
                Optional.empty(),
                Optional.empty());
          } else {
            InputStream inputStream = response.body();
            var metadata = new Metadata(response.headers(), response.statusCode());
            var sizeMaybe = getResponseDataSize(response.headers());
            int ttl = calculateTTL(response.headers());
            return new Thing(inputStream, metadata, sizeMaybe , Optional.of(ttl));
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
            maxAge = Integer.parseInt(maxAgeBinding[1]);
          }
          break;
        }
      }
    }
    return maxAge;
  }

  public interface RequestMaker {
      EnsoHttpResponse run() throws IOException, InterruptedException;

      String hashKey();

      EnsoHttpResponse reconstructResponseFromCachedStream(InputStream inputStream, CacheResult cacheResult);
  }

  public record Metadata(HttpHeaders headers, int statusCode) {}
}
