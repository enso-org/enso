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
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.function.Predicate;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.Map;
import java.util.Optional;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Collectors;

import org.enso.base.net.URIWithSecrets;
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
  TransientHTTPResponseCache() {}
  private static final Logger logger = Logger.getLogger(TransientHTTPResponseCache .class.getName());

  private static final int DEFAULT_TTL_SECONDS = 31536000;
  private static long MAX_FILE_SIZE = 10L * 1024 * 1024;
  private static long MAX_TOTAL_CACHE_SIZE = 10L * 1024 * 1024 * 1024;

  /**
   * This value is used for the current time when deciding if a cache entry is
   * stale.
   */
  private static ZonedDateTime nowOverrideTestOnly = null;

  /**
   * Used for testing file and cache size limits. These cannot be set to values
   * larger than the real limits.
   */
  private static Optional<Long> maxFileSizeOverrideTestOnly = Optional.empty();
  private static Optional<Long> maxTotalCacheSizeOverrideTestOnly  = Optional.empty();

  private static final Map<String, CacheEntry> cache = new HashMap<>();

  static EnsoHttpResponse makeRequest(
      URIWithSecrets unresolvedURI,
      URI resolvedURI,
      List<Pair<String, String>> resolvedHeaders,
      RequestMaker requestMaker)
      throws IOException, InterruptedException, ResponseTooLargeException {
    removeStaleEntries();

    var cacheKey = makeHashKey(resolvedURI, resolvedHeaders);
    //System.out.println("AAA cache hit " + cacheKey + " " + cache.containsKey(cacheKey));
    if (cache.containsKey(cacheKey)) {
      return returnCachedResponse(cacheKey);
    } else {
      return makeRequestAndCache(unresolvedURI, resolvedURI, resolvedHeaders, cacheKey, requestMaker);
    }
  }

  private static EnsoHttpResponse returnCachedResponse(String cacheKey) throws IOException {
    return buildEnsoHttpResponseFromCacheEntry(cache.get(cacheKey));
  }

  // IOExceptions thrown by the HTTP request are propagated; IOExceptions thrown
  // while storing the data in the cache are caught.
  private static EnsoHttpResponse makeRequestAndCache(
      URIWithSecrets unresolvedURI,
      URI resolvedURI,
      List<Pair<String, String>> resolvedHeaders,
      String cacheKey,
      RequestMaker requestMaker) throws InterruptedException, IOException, ResponseTooLargeException {
    assert !cache.containsKey(cacheKey);

    var response = requestMaker.run();

    if (response.statusCode() != 200) {
      return response;
    }

    var sizeMaybe = response.headers().firstValue("content-length").map(Long::parseLong);
    if (sizeMaybe.isPresent()) {
      long size = sizeMaybe.get();
      if (size > getMaxFileSize()) {
        throw new ResponseTooLargeException(size, getMaxFileSize(), unresolvedURI.toString());
      }
      makeRoomFor(size);
    }

    try {
      int ttl = calculateTTL(response.headers());
      ZonedDateTime expiry = getNow().plus(Duration.ofSeconds(ttl));
      int statusCode = response.statusCode();
      var headers = response.headers();
      String responseDataPath = downloadResponseData(response);
      long size = new File(responseDataPath).length();

      if (sizeMaybe.isPresent() && size != sizeMaybe.get()) {
        // If the file is larger than expected, we might have to remove some more old entries.
        logger.log(Level.WARNING, "Downloaded size " + size + " != content-length value " + sizeMaybe.get());
        removeFilesToSatisfyLimit();
      }

      var cacheEntry = new CacheEntry(resolvedURI, headers, responseDataPath, size, statusCode, expiry);

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
    var now = getNow();
    System.out.println("AAA cleanup " + nowOverrideTestOnly);
    //System.out.println("AAA cleanup " + cache);
    if (nowOverrideTestOnly != null) {
      for (var ce : cache.values()) {
        var ex = ce.expiry();
        //System.out.println("AAA ce " + ex + " / " + Duration.between(ex, nowOverrAideTestOnly));
      }
    }
    //removeCacheEntries(e -> e.expiry().isBefore(now));
    removeCacheEntries(e -> {
      var stale = e.expiry().isBefore(now);
      var ex = e.expiry();
      if (nowOverrideTestOnly != null) {
        System.out.println("AAA check stale " + stale + " " + Duration.between(nowOverrideTestOnly, ex) + " " + nowOverrideTestOnly + " " + ex);
      }
      return stale;
    });
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
      boolean shouldRemove = predicate.test(cacheValue);
      if (shouldRemove) {
        System.out.println("AAA removing " + cacheValue);
        it.remove();
        removeCacheFile(key, cacheValue);
      }
    }
    //System.out.println("AAA");
  }

  private static void removeCacheFile(String key, CacheEntry cacheEntry) {
    File file = new File(cacheEntry.responseDataPath);
    boolean removed = file.delete();
    if (!removed) {
      logger.log(Level.WARNING, "Unable to delete cache file for key {0}", key);
    }
  }

  /**
   * Remove old entries until there is enough room for a new file.
   */
  private static void makeRoomFor(long newFileSize) {
    long totalSize = getTotalCacheSize() + newFileSize;
    long maxTotalCacheSize = getMaxTotalCacheSize();
    if (totalSize <= maxTotalCacheSize) {
      return;
    }
    var sortedEntries = getSortedEntries();
    var toRemove = new ArrayList<Map.Entry<String, CacheEntry>>();
    for (var mapEntry : sortedEntries) {
      System.out.println("AAA entry " + mapEntry.getValue().expiry() + " " + mapEntry);
    }
    for (var mapEntry : sortedEntries) {
      if (totalSize <= maxTotalCacheSize) {
        break;
      }
      toRemove.add(mapEntry);
      totalSize -= mapEntry.getValue().size();
      System.out.println("AAA will remove " + mapEntry.getValue().expiry() + " " + mapEntry.getValue().size() + " " + totalSize + " " + mapEntry);
    }
    assert totalSize <= maxTotalCacheSize;
    for (var mapEntry : toRemove) {
      cache.remove(mapEntry.getKey());
      removeCacheFile(mapEntry.getKey(), mapEntry.getValue());
    }
    System.out.println("AAA total now " + getTotalCacheSize() + " " + maxTotalCacheSize);
  }

  private static SortedSet<Map.Entry<String, CacheEntry>> getSortedEntries() {
    var sortedEntries = new TreeSet<Map.Entry<String, CacheEntry>>(cacheEntryExpiryComparatorDesc);
    sortedEntries.addAll(cache.entrySet());
    return sortedEntries;
  }

  /**
   * Remove old entries until the total cache size is under the limit.
   */
  private static void removeFilesToSatisfyLimit() {
    makeRoomFor(0L);
  }

  private static long getTotalCacheSize() {
    return cache.values().stream().collect(Collectors.summingLong(e -> e.size()));
  }

  private static long getMaxFileSize() {
    return maxFileSizeOverrideTestOnly.orElse(MAX_FILE_SIZE);
  }

  private static long getMaxTotalCacheSize() {
    return maxTotalCacheSizeOverrideTestOnly.orElse(MAX_TOTAL_CACHE_SIZE);
  }

  public static int getNumEntries() {
    return cache.size();
  }

  public static List<Long> getFileSizesTestOnly() {
    return new ArrayList<>(cache.values().stream().map(CacheEntry::size).collect(Collectors.toList()));
  }

  public static void setNowOverrideTestOnly(ZonedDateTime nowOverride) {
    //System.out.println("AAA zdt " + nowOverride);
    nowOverrideTestOnly = nowOverride;
  }

  public static void setMaxFileSizeOverrideTestOnly(long maxFileSizeOverrideTestOnly_) {
    assert maxFileSizeOverrideTestOnly_ <= MAX_FILE_SIZE;
    maxFileSizeOverrideTestOnly = Optional.of(maxFileSizeOverrideTestOnly_);
  }

  public static void clearMaxFileSizeOverrideTestOnly() {
    maxFileSizeOverrideTestOnly = Optional.empty();
  }

  public static void setMaxTotalCacheSizeOverrideTestOnly(long maxTotalCacheSizeOverrideTestOnly_) {
    assert maxTotalCacheSizeOverrideTestOnly_ <= MAX_TOTAL_CACHE_SIZE;
    maxTotalCacheSizeOverrideTestOnly = Optional.of(maxTotalCacheSizeOverrideTestOnly_);
  }

  public static void clearMaxTotalCacheSizeOverrideTestOnly() {
    maxTotalCacheSizeOverrideTestOnly = Optional.empty();
  }

  private static ZonedDateTime getNow() {
    // The 'nowOverrideTestOnly ' field is used for testing TTL expiration logic.
    if (nowOverrideTestOnly != null) {
      return nowOverrideTestOnly;
    } else {
      return ZonedDateTime.now();
    }
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
    //System.out.println("AAA h "+headers.map());
    //System.out.println("AAA "+headers.firstValue("asdf"));
    Integer maxAge = getMaxAge(headers);
    if (maxAge == null) {
      return DEFAULT_TTL_SECONDS;
    } else {
      int age = headers.firstValue("age").map(Integer::parseInt).orElse(0);
      System.out.println("AAA calculateTTL " + maxAge + " " + age);
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
      //System.out.println("AAA uri " + resolvedURI.toString());
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

  private record CacheEntry(URI uri, HttpHeaders headers, String responseDataPath, long size, int statusCode, ZonedDateTime expiry) {}

  //private static final Comparator<Map.Entry<String, CacheEntry>> cacheEntryExpiryComparatorDesc = Comparator.comparing(me -> me.getValue.expiry()).reversed();

  private static final ZonedDateTime getMapEntryExpiry(Map.Entry<String, CacheEntry> mapEntry) {
    return mapEntry.getValue().expiry();
  }

  private static final Comparator<Map.Entry<String, CacheEntry>> cacheEntryExpiryComparatorDesc = Comparator.comparing(TransientHTTPResponseCache::getMapEntryExpiry).reversed();
}
