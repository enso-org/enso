package org.enso.base.enso_cloud;

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
import org.enso.base.net.URIWithSecrets;
import org.graalvm.collections.Pair;

/**
 * TransientHTTPResponseCache is a cache for EnsoHttpResponse values that respects the cache control
 * HTTP headers received in the original repsonse to a request.
 *
 * <p>It also puts limits on the size of files that can be requested, and on the total cache size,
 * deleting entries to make space for new ones.
 *
 * <p>Without cachineg, EnsoHttpResponse contains an InputStream providing the response data. When
 * there is a cache hit, this stream reads from the local file storing the cached data. When there
 * is no cache hit, the InputStream is connected directly to the remote server.
 */
public class TransientHTTPResponseCache {
  TransientHTTPResponseCache() {}

  private static final Logger logger = Logger.getLogger(TransientHTTPResponseCache.class.getName());

  // 1 year.
  private static final int DEFAULT_TTL_SECONDS = 31536000;

  private static long MAX_FILE_SIZE = 10L * 1024 * 1024;
  private static long MAX_TOTAL_CACHE_SIZE = 10L * 1024 * 1024 * 1024;

  /** This value is used for the current time when deciding if a cache entry is stale. */
  private static ZonedDateTime nowOverrideTestOnly = null;

  /**
   * Used for testing file and cache size limits. These cannot be set to values larger than the real
   * limits.
   */
  private static Optional<Long> maxFileSizeOverrideTestOnly = Optional.empty();

  private static Optional<Long> maxTotalCacheSizeOverrideTestOnly = Optional.empty();

  private static final Map<String, CacheEntry> cache = new HashMap<>();
  private static final Map<String, ZonedDateTime> lastUsed = new HashMap<>();

  static EnsoHttpResponse makeRequest(
      URIWithSecrets unresolvedURI,
      URI resolvedURI,
      List<Pair<String, String>> resolvedHeaders,
      RequestMaker requestMaker)
      throws IOException, InterruptedException, ResponseTooLargeException {
    removeStaleEntries();

    var cacheKey = makeHashKey(resolvedURI, resolvedHeaders);
    if (cache.containsKey(cacheKey)) {
      return returnCachedResponse(cacheKey);
    } else {
      return makeRequestAndCache(
          unresolvedURI, resolvedURI, resolvedHeaders, cacheKey, requestMaker);
    }
  }

  private static EnsoHttpResponse returnCachedResponse(String cacheKey) throws IOException {
    markCacheEntryUsed(cacheKey);
    return buildEnsoHttpResponseFromCacheEntry(cache.get(cacheKey));
  }

  /**
   * IOExceptions thrown by the HTTP request are propagated; IOExceptions thrown while storing the
   * data in the cache are caught.
   *
   * <p>Only HTTP 200 responses are cached; all others are returned uncached.
   */
  private static EnsoHttpResponse makeRequestAndCache(
      URIWithSecrets unresolvedURI,
      URI resolvedURI,
      List<Pair<String, String>> resolvedHeaders,
      String cacheKey,
      RequestMaker requestMaker)
      throws InterruptedException, IOException, ResponseTooLargeException {
    assert !cache.containsKey(cacheKey);

    var response = requestMaker.run();

    // Don't cache non-200 repsonses.
    if (response.statusCode() != 200) {
      return response;
    }

    // If we get a content-length header, make room for the new file before
    // downloading. If the response is too large, throw
    // ResponseTooLargeException.
    var sizeMaybe = response.headers().firstValue("content-length").map(Long::parseLong);
    if (sizeMaybe.isPresent()) {
      long size = sizeMaybe.get();
      if (size > getMaxFileSize()) {
        throw new ResponseTooLargeException(
            size, getMaxFileSize(), unresolvedURI.render().toString());
      }
      makeRoomFor(size);
    }

    try {
      // Download the response data.
      int ttl = calculateTTL(response.headers());
      ZonedDateTime expiry = getNow().plus(Duration.ofSeconds(ttl));
      int statusCode = response.statusCode();
      var headers = response.headers();
      String responseDataPath = downloadResponseData(unresolvedURI, response);
      long size = new File(responseDataPath).length();

      // Create a cache entry. Remove old files if we are over the cache size
      // limit.
      var cacheEntry =
          new CacheEntry(resolvedURI, headers, responseDataPath, size, statusCode, expiry);
      cache.put(cacheKey, cacheEntry);
      markCacheEntryUsed(cacheKey);

      // Clear out old entries to satisfy the total cache size limit. This might
      // be necessary here if we didn't receive a content-length.
      removeFilesToSatisfyLimit();

      return buildEnsoHttpResponseFromCacheEntry(cacheEntry);
    } catch (IOException e) {
      logger.log(
          Level.WARNING,
          "Failure when caching HTTP response, will re-issue request without caching: " + e);
      // Re-issue the request since we don't know if we've consumed any of the response.
      return requestMaker.run();
    }
  }

  /** Create an EnsoHttpResponse with an InputStream reading from the cache file. */
  private static EnsoHttpResponse buildEnsoHttpResponseFromCacheEntry(CacheEntry cacheEntry)
      throws IOException {
    InputStream inputStream = new FileInputStream(cacheEntry.responseDataPath);
    return new EnsoHttpResponse(
        cacheEntry.uri(), cacheEntry.headers(), inputStream, cacheEntry.statusCode());
  }

  /**
   * Read the repsonse data from the remote server into the cache file. If the downloaded data is
   * over the file size limit, throw a ResponseTooLargeException.
   */
  private static String downloadResponseData(
      URIWithSecrets unresolvedURI, EnsoHttpResponse response)
      throws IOException, ResponseTooLargeException {
    File temp = File.createTempFile("TransientHTTPResponseCache", "");
    try {
      var inputStream = response.body();
      var outputStream = new FileOutputStream(temp);

      // Limit the download to getMaxFileSize().
      long tooMany = getMaxFileSize() + 1;
      long bytesCopied = IOUtils.copyLarge(inputStream, outputStream, 0, tooMany);
      if (bytesCopied >= tooMany) {
        try {
          temp.delete();
          outputStream.close();
        } finally {
          // catch block below will delete the temp file.
          throw new ResponseTooLargeException(
              tooMany, getMaxFileSize(), unresolvedURI.render().toString());
        }
      }

      outputStream.close();
      temp.deleteOnExit();
      return temp.getAbsolutePath();
    } catch (IOException e) {
      temp.delete();
      throw e;
    }
  }

  /** Mark the entry with the current time, to maintain LRU data. */
  private static void markCacheEntryUsed(String cacheKey) {
    lastUsed.put(cacheKey, ZonedDateTime.now());
  }

  /** Remove all cache entries (and their files) that have passed their TTL. */
  private static void removeStaleEntries() {
    var now = getNow();
    removeCacheEntriesByPredicate(e -> e.expiry().isBefore(now));
  }

  /** Remove all cache entries (and their files). */
  public static void clear() {
    removeCacheEntriesByPredicate(e -> true);
  }

  /** Remove all cache entries (and their cache files) that match the predicate. */
  private static void removeCacheEntriesByPredicate(Predicate<CacheEntry> predicate) {
    List<Map.Entry<String, CacheEntry>> toRemove =
        cache.entrySet().stream()
            .filter(me -> predicate.test(me.getValue()))
            .collect(Collectors.toList());
    removeCacheEntries(toRemove);
  }

  /** Remove a set of cache entries. */
  private static void removeCacheEntries(List<Map.Entry<String, CacheEntry>> toRemove) {
    for (var entry : toRemove) {
      removeCacheEntry(entry);
    }
  }

  /** Remove a cache entry: from `cache`, `lastUsed`, and the filesystem. */
  private static void removeCacheEntry(Map.Entry<String, CacheEntry> toRemove) {
    var key = toRemove.getKey();
    var value = toRemove.getValue();
    cache.remove(key);
    lastUsed.remove(key);
    removeCacheFile(key, value);
  }

  /** Remove a cache file. */
  private static void removeCacheFile(String key, CacheEntry cacheEntry) {
    File file = new File(cacheEntry.responseDataPath);
    boolean removed = file.delete();
    if (!removed) {
      logger.log(Level.WARNING, "Unable to delete cache file for key {0}", key);
    }
  }

  /** Remove least-recently used entries until there is enough room for a new file. */
  private static void makeRoomFor(long newFileSize) {
    long totalSize = getTotalCacheSize() + newFileSize;
    long maxTotalCacheSize = getMaxTotalCacheSize();
    if (totalSize <= maxTotalCacheSize) {
      return;
    }

    // Remove least-recently used entries first.
    var sortedEntries = getSortedEntries();
    var toRemove = new ArrayList<Map.Entry<String, CacheEntry>>();
    for (var mapEntry : sortedEntries) {
      if (totalSize <= maxTotalCacheSize) {
        break;
      }
      toRemove.add(mapEntry);
      totalSize -= mapEntry.getValue().size();
    }
    assert totalSize <= maxTotalCacheSize;
    removeCacheEntries(toRemove);
  }

  private static SortedSet<Map.Entry<String, CacheEntry>> getSortedEntries() {
    var sortedEntries = new TreeSet<Map.Entry<String, CacheEntry>>(cacheEntryLRUComparator);
    sortedEntries.addAll(cache.entrySet());
    return sortedEntries;
  }

  /** Remove least-recently used entries until the total cache size is under the limit. */
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
    return new ArrayList<>(
        cache.values().stream().map(CacheEntry::size).collect(Collectors.toList()));
  }

  public static void setNowOverrideTestOnly(ZonedDateTime nowOverride) {
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

  /**
   * Sorts the header by header name, so we don't depend on header order. Multiple-valued headers
   * might hash differently, but it's a rare case.
   */
  private static String makeHashKey(URI resolvedURI, List<Pair<String, String>> resolvedHeaders) {
    try {
      MessageDigest messageDigest = MessageDigest.getInstance("SHA-256");
      messageDigest.update(resolvedURI.toString().getBytes());

      var sortedHeaders = resolvedHeaders.stream().sorted(headerNameComparator).toList();
      for (Pair<String, String> resolvedHeader : sortedHeaders) {
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

  private record CacheEntry(
      URI uri,
      HttpHeaders headers,
      String responseDataPath,
      long size,
      int statusCode,
      ZonedDateTime expiry) {}

  private static final Comparator<Map.Entry<String, CacheEntry>> cacheEntryLRUComparator =
      Comparator.comparing(me -> lastUsed.get(me.getKey()));
  private static final Comparator<Pair<String, String>> headerNameComparator =
      Comparator.comparing(pair -> pair.getLeft());
}
