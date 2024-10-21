package org.enso.base.cache;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
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

public class StreamCache<M> {
  private static final Logger logger = Logger.getLogger(StreamCache .class.getName());

  private final long MAX_FILE_SIZE = 10L * 1024 * 1024;
  private final long MAX_TOTAL_CACHE_SIZE = 10L * 1024 * 1024 * 1024;

  /** This value is used for the current time when deciding if a cache entry is stale. */
  private ZonedDateTime nowOverrideTestOnly = null;

  /**
   * Used for testing file and cache size limits. These cannot be set to values larger than the real
   * limits.
   */
  private Optional<Long> maxFileSizeOverrideTestOnly = Optional.empty();

  private Optional<Long> maxTotalCacheSizeOverrideTestOnly = Optional.empty();

  private final Map<String, CacheEntry<M>> cache = new HashMap<>();
  private final Map<String, ZonedDateTime> lastUsed = new HashMap<>();

  public CacheResult<M> getResult(StreamMaker<M> streamMaker) throws IOException, ResponseTooLargeException {
      String cacheKey = streamMaker.makeCacheKey();
      if (cache.containsKey(cacheKey)) {
        return getResultForCacheEntry(cacheKey);
      } else {
        return makeRequestAndCache(cacheKey, streamMaker);
      }
  }

  private CacheResult<M> makeRequestAndCache(String cacheKey, StreamMaker<M> streamMaker) throws ResponseTooLargeException {
    assert !cache.containsKey(cacheKey);

    Thing<M> thing = streamMaker.makeThing();

    if (!thing.shouldCache()) {
        return new CacheResult<>(thing.stream(), thing.metadata());
    }

    if (thing.sizeMaybe.isPresent()) {
      long size = thing.sizeMaybe().get();
      if (size > getMaxFileSize()) {
        throw new ResponseTooLargeException(size, getMaxFileSize());
      }
      makeRoomFor(size);
    }

    try {
      // Download the response data.
      File responseData = downloadResponseData(cacheKey, thing);
      M metadata = thing.metadata();
      long size = responseData.length();
      ZonedDateTime expiry = getNow().plus(Duration.ofSeconds(thing.ttl().get()));

      // Create a cache entry.
      var cacheEntry = new CacheEntry<>(responseData, metadata, size, expiry);
      cache.put(cacheKey, cacheEntry);
      markCacheEntryUsed(cacheKey);

      // Clear out old entries to satisfy the total cache size limit. This might
      // be necessary here if we didn't receive a correct content size value.
      removeFilesToSatisfyLimit();

      return getResultForCacheEntry(cacheKey);
    } catch (IOException e) {
      logger.log(
          Level.WARNING,
          "Failure storing cache entry; will re-execute without caching: " + e);
      // Re-issue the request since we don't know if we've consumed any of the response.
      Thing<M> rerequested = streamMaker.makeThing();
      return new CacheResult<>(rerequested .stream(), rerequested .metadata());
    }
  }

  /** Mark cache entry used and return a stream reading from the cache file. */
  private CacheResult<M> getResultForCacheEntry(String cacheKey) throws IOException {
    markCacheEntryUsed(cacheKey);
    return new CacheResult<>(
        new FileInputStream(cache.get(cacheKey).responseData),
        cache.get(cacheKey).metadata());
  }

  /**
   * Read the repsonse data from the remote server into the cache file. If the downloaded data is
   * over the file size limit, throw a ResponseTooLargeException.
   */
  private File downloadResponseData(String cacheKey, Thing thing)
      throws IOException, ResponseTooLargeException {
    File temp = File.createTempFile("TransientHTTPResponseCache-" + cacheKey, "");
    temp.deleteOnExit();
    try {
      var inputStream = thing.stream();
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
          throw new ResponseTooLargeException(tooMany, getMaxFileSize());
        }
      }

      outputStream.close();
      return temp;
    } catch (IOException e) {
      temp.delete();
      throw e;
    }
  }

  /** Mark the entry with the current time, to maintain LRU data. */
  private void markCacheEntryUsed(String cacheKey) {
    lastUsed.put(cacheKey, ZonedDateTime.now());
  }

  /** Remove all cache entries (and their files) that have passed their TTL. */
  private void removeStaleEntries() {
    var now = getNow();
    removeCacheEntriesByPredicate(e -> e.expiry().isBefore(now));
  }

  /** Remove all cache entries (and their files). */
  public void clear() {
    removeCacheEntriesByPredicate(e -> true);
  }

  /** Remove all cache entries (and their cache files) that match the predicate. */
  private void removeCacheEntriesByPredicate(Predicate<CacheEntry<M>> predicate) {
    List<Map.Entry<String, CacheEntry<M>>> toRemove =
        cache.entrySet().stream()
            .filter(me -> predicate.test(me.getValue()))
            .collect(Collectors.toList());
    removeCacheEntries(toRemove);
  }

  /** Remove a set of cache entries. */
  private void removeCacheEntries(List<Map.Entry<String, CacheEntry<M>>> toRemove) {
    for (var entry : toRemove) {
      removeCacheEntry(entry);
    }
  }

  /** Remove a cache entry: from `cache`, `lastUsed`, and the filesystem. */
  private void removeCacheEntry(Map.Entry<String, CacheEntry<M>> toRemove) {
    var key = toRemove.getKey();
    var value = toRemove.getValue();
    cache.remove(key);
    lastUsed.remove(key);
    removeCacheFile(key, value);
  }

  /** Remove a cache file. */
  private void removeCacheFile(String key, CacheEntry<M> cacheEntry) {
    boolean removed = cacheEntry.responseData.delete();
    if (!removed) {
      logger.log(Level.WARNING, "Unable to delete cache file for key {0}", key);
    }
  }

  /** Remove least-recently used entries until there is enough room for a new file. */
  private void makeRoomFor(long newFileSize) {
    removeStaleEntries();

    long totalSize = getTotalCacheSize() + newFileSize;
    long maxTotalCacheSize = getMaxTotalCacheSize();
    if (totalSize <= maxTotalCacheSize) {
      return;
    }

    // Remove least-recently used entries first.
    var sortedEntries = getSortedEntries();
    var toRemove = new ArrayList<Map.Entry<String, CacheEntry<M>>>();
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

  private SortedSet<Map.Entry<String, CacheEntry<M>>> getSortedEntries() {
    var sortedEntries = new TreeSet<Map.Entry<String, CacheEntry<M>>>(cacheEntryLRUComparator);
    sortedEntries.addAll(cache.entrySet());
    return sortedEntries;
  }

  /** Remove least-recently used entries until the total cache size is under the limit. */
  private void removeFilesToSatisfyLimit() {
    makeRoomFor(0L);
  }

  private long getTotalCacheSize() {
    return cache.values().stream().collect(Collectors.summingLong(e -> e.size()));
  }

  private long getMaxFileSize() {
    return maxFileSizeOverrideTestOnly.orElse(MAX_FILE_SIZE);
  }

  private long getMaxTotalCacheSize() {
    return maxTotalCacheSizeOverrideTestOnly.orElse(MAX_TOTAL_CACHE_SIZE);
  }

  public int getNumEntries() {
    return cache.size();
  }

  public List<Long> getFileSizesTestOnly() {
    return new ArrayList<>(
        cache.values().stream().map(CacheEntry::size).collect(Collectors.toList()));
  }

  public void setNowOverrideTestOnly(ZonedDateTime nowOverride) {
    nowOverrideTestOnly = nowOverride;
  }

  public void setMaxFileSizeOverrideTestOnly(long maxFileSizeOverrideTestOnly_) {
    assert maxFileSizeOverrideTestOnly_ <= MAX_FILE_SIZE;
    maxFileSizeOverrideTestOnly = Optional.of(maxFileSizeOverrideTestOnly_);
  }

  public void clearMaxFileSizeOverrideTestOnly() {
    maxFileSizeOverrideTestOnly = Optional.empty();
  }

  public void setMaxTotalCacheSizeOverrideTestOnly(long maxTotalCacheSizeOverrideTestOnly_) {
    assert maxTotalCacheSizeOverrideTestOnly_ <= MAX_TOTAL_CACHE_SIZE;
    maxTotalCacheSizeOverrideTestOnly = Optional.of(maxTotalCacheSizeOverrideTestOnly_);
  }

  public void clearMaxTotalCacheSizeOverrideTestOnly() {
    maxTotalCacheSizeOverrideTestOnly = Optional.empty();
  }

  private ZonedDateTime getNow() {
    // The 'nowOverrideTestOnly ' field is used for testing TTL expiration logic.
    if (nowOverrideTestOnly != null) {
      return nowOverrideTestOnly;
    } else {
      return ZonedDateTime.now();
    }
  }

  private record CacheEntry<M> (
      File responseData,
      M metadata,
      long size,
      ZonedDateTime expiry) {}

  public record Thing<M> (
      InputStream stream,
      Optional<Long> sizeMaybe,
      Optional<Integer> ttl,
      M metadata) {

      public boolean shouldCache() {
        return ttl.isPresent();
      }
    }

  public record CacheResult<M> (
    InputStream inputStream,
    M metadata) {}

  public interface StreamMaker<M> {
      String makeCacheKey();
      Thing<M> makeThing();
  }

  private final Comparator<Map.Entry<String, CacheEntry<M>>> cacheEntryLRUComparator =
      Comparator.comparing(me -> lastUsed.get(me.getKey()));
}
