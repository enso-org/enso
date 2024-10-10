package org.enso.base.enso_cloud;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.http.HttpHeaders;
import java.net.URI;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Iterator;
import java.util.function.Predicate;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.enso.base.enso_cloud.EnsoHttpResponse;
import org.graalvm.collections.Pair;

/**
 * TransientHTTPResponseCache is a cache for EnsoHttpResponse values that
 * respects the cache control HTTP headers received in the original repsonse to
 * a request.
 * 
 * EnsoHttpResponse contains an InputStream providing the response data. When
 * there is a cache hit, this stream reads from the local file storing the
 * cached data. When there is no cache hit, the InputStream is connected
 * directly to the remote server.
 */
public class TransientHTTPResponseCache {
  private static final Logger logger = Logger.getLogger(TransientHTTPResponseCache .class.getName());

  private static final int DEFAULT_TTL_SECONDS = 31536000;

  /**
   * This value is added to the current time when deciding if a cache entry is
   * stale.
   */
  private static int advanceSecsTestOnly = 0;

  private static final Map<String, CacheEntry> cache = new HashMap<>();

  static EnsoHttpResponse makeRequest(
      URI resolvedURI,
      List<Pair<String, String>> resolvedHeaders,
      RequestMaker requestMaker)
      throws IOException, InterruptedException {
    removeStaleEntries();

    var cacheKey = makeHashKey(resolvedURI, resolvedHeaders);
    System.out.println("AAA cache hit " + cacheKey + " " + cache.containsKey(cacheKey));
    if (cache.containsKey(cacheKey)) {
      return returnCachedResponse(cacheKey);
    } else {
      return makeRequestAndCache(resolvedURI, resolvedHeaders, cacheKey, requestMaker);
    }
  }

  private static EnsoHttpResponse returnCachedResponse(String cacheKey) throws IOException {
    return buildEnsoHttpResponseFromCacheEntry(cache.get(cacheKey));
  }

  // IOExceptions thrown by the HTTP request are propagated; IOExceptions thrown
  // while storing the data in the cache are caught.
  private static EnsoHttpResponse makeRequestAndCache(
      URI resolvedURI,
      List<Pair<String, String>> resolvedHeaders,
      String cacheKey,
      RequestMaker requestMaker) throws InterruptedException, IOException {
    assert !cache.containsKey(cacheKey);

    var response = requestMaker.run();

    if (response.statusCode() != 200) {
      return response;
    }

    try {
      int ttl = calculateTTL(response.headers());
      LocalDateTime expiry = LocalDateTime.now().plus(Duration.ofSeconds(ttl));
      int statusCode = response.statusCode();
      var headers = response.headers();
      String responseDataPath = downloadResponseData(response);

      var cacheEntry = new CacheEntry(resolvedURI, headers, responseDataPath, statusCode, expiry);

      cache.put(cacheKey, cacheEntry);

      return buildEnsoHttpResponseFromCacheEntry(cacheEntry);
    } catch (IOException e) {
      // Re-issue the request since we don't know if we've consumed any of the response.
      return requestMaker.run();
    }
  }

  private static EnsoHttpResponse buildEnsoHttpResponseFromCacheEntry(CacheEntry cacheEntry) throws IOException {
    InputStream inputStream = new FileInputStream(cacheEntry.responseDataPath);
    return new EnsoHttpResponse(cacheEntry.uri(), cacheEntry.headers(), inputStream , cacheEntry.statusCode());
  }

  private static String downloadResponseData(EnsoHttpResponse response) throws IOException {
    File temp = File.createTempFile("TransientHTTPResponseCache", "");
    try {
      var outputStream = new FileOutputStream(temp);
      response.body().transferTo(outputStream);
      temp.deleteOnExit();
      return temp.getAbsolutePath();
    } catch (IOException e) {
      temp.delete();
      throw e;
    }
  }

  /** Remove all cache entries (and their files) that have passed their TTL. */
  private static void removeStaleEntries() {
    // The 'advanceSecsTestOnly' field is used for testing TTL expiration logic.
    var now = LocalDateTime.now().plus(Duration.ofSeconds(advanceSecsTestOnly));
    removeCacheEntries(e -> e.expiry().isBefore(now));
  }

  /** Remove all cache entries (and their files). */
  public static void clear() {
    removeCacheEntries(e -> true);
  }

  /** Remove all cache entries (and their cache files) that match the predicate. */
  private static void removeCacheEntries(Predicate<CacheEntry> predicate) {
    for (Iterator<Map.Entry<String, CacheEntry>> it = cache.entrySet().iterator(); it.hasNext();) {
      var entry = it.next();
      var key = entry.getKey();
      var cacheValue = entry.getValue();
      boolean isStale = predicate.test(cacheValue);
      if (isStale) {
        it.remove();
        removeCacheFile(key, cacheValue);
      }
    }
    System.out.println("AAA");
  }

  private static void removeCacheFile(String key, CacheEntry cacheEntry) {
    File file = new File(cacheEntry.responseDataPath);
    boolean removed = file.delete();
    if (!removed) {
      logger.log(Level.WARNING, "Unable to delete cache file for key {0}", key);
    }
  }

  public static int getNumEntries() {
    return cache.size();
  }

  public static void setTimeAdvanceTestOnly(int advanceSecs) {
    advanceSecsTestOnly = advanceSecs;
  }

  /**
   * We define the TTL as the amount of time that the response should be considered fresh.
   * 
   * Define t0 as the time at which the content was generated on the origin
   * server.
   * 
   * Define t1 as the time at which the current request was handled, either by
   * the origin server or an intervening cache.
   * 
   * The 'Age' header, if present is (t1 - t0).
   * 
   * The 'max-age' value in the 'Cache-Control' header, if present, is the
   * origin server's definition of how long the response should be considered
   * fresh.
   * 
   * If 'max-age' and 'Age' are both present, we set TTL = max-age - Age.
   * If only 'max-age' is present, we set TTL = max-age.
   * If neither are present, we use a default.
   */
  private static int calculateTTL(HttpHeaders headers) {
    System.out.println("AAA h "+headers.map());
    System.out.println("AAA "+headers.firstValue("asdf"));
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

  private static String makeHashKey(
      URI resolvedURI,
      List<Pair<String, String>> resolvedHeaders) {
    try {
      MessageDigest messageDigest = MessageDigest.getInstance("SHA-256");
      System.out.println("AAA uri " + resolvedURI.toString());
      messageDigest.update(resolvedURI.toString().getBytes());
      for (Pair<String, String> resolvedHeader : resolvedHeaders) {
        System.out.println("AAA header "+resolvedHeader.getLeft()+" "+resolvedHeader.getRight());
        messageDigest.update(resolvedHeader.getLeft().getBytes());
        messageDigest.update(resolvedHeader.getRight().getBytes());
      }
      return toHexString(messageDigest.digest());
    } catch (NoSuchAlgorithmException ex) {
      throw raise(RuntimeException.class, ex);
    }
  }

  public static String toHexString(byte[] bytes) {
    char[] out = new char[bytes.length * 2];
    for (int i = 0; i < bytes.length; i++) {
      int v = bytes[i] & 0xFF;
      out[i * 2] = Character.forDigit(v >>> 4, 16);
      out[i * 2 + 1] = Character.forDigit(v & 0x0F, 16);
    }
    return new String(out);
  }

  @FunctionalInterface
  public interface RequestMaker {
    EnsoHttpResponse run() throws IOException, InterruptedException;
  }

  @SuppressWarnings("unchecked")
  private static <E extends Exception> E raise(Class<E> type, Exception ex) throws E {
    throw (E) ex;
  }

  private record CacheEntry(URI uri, HttpHeaders headers, String responseDataPath, int statusCode, LocalDateTime expiry) {}
}
