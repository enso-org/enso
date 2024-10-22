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
import org.enso.base.Stream_Utils;

/**
 * LRUCache is a cache for data presented via InputStreams. Files are deleted on JVM exit.
 *
 * <p>It puts limits on the size of files that can be requested, and on the total cache size,
 * deleting entries to make space for new ones. All cache files are set to be deleted automatically
 * on JVM exit.
 *
 * @param <M> Additional metadata to associate with the data.
 */
public class LRUCache<M> {
  private static final Logger logger = Logger.getLogger(LRUCache.class.getName());

  private final long maxFileSize;
  private final long maxTotalCacheSize;

  /** This value is used for the current time when testing TTL expiration logic. */
  private Optional<ZonedDateTime> nowOverrideTestOnly = Optional.empty();

  /**
   * Used for testing file and cache size limits. These cannot be set to values larger than the real
   * limits.
   */
  private Optional<Long> maxFileSizeOverrideTestOnly = Optional.empty();

  private Optional<Long> maxTotalCacheSizeOverrideTestOnly = Optional.empty();

  private final Map<String, CacheEntry<M>> cache = new HashMap<>();
  private final Map<String, ZonedDateTime> lastUsed = new HashMap<>();

  public LRUCache(long maxFileSize, long maxTotalCacheSize) {
    this.maxFileSize = maxFileSize;
    this.maxTotalCacheSize = maxTotalCacheSize;
  }

  public CacheResult<M> getResult(ItemBuilder<M> itemBuilder)
      throws IOException, InterruptedException, ResponseTooLargeException {
    String cacheKey = itemBuilder.makeCacheKey();
    if (cache.containsKey(cacheKey)) {
      return getResultForCacheEntry(cacheKey);
    } else {
      return makeRequestAndCache(cacheKey, itemBuilder);
    }
  }

  /**
   * IOExceptions thrown by the HTTP request are propagated; IOExceptions thrown while storing the
   * data in the cache are caught.
   */
  private CacheResult<M> makeRequestAndCache(String cacheKey, ItemBuilder<M> itemBuilder)
      throws IOException, InterruptedException, ResponseTooLargeException {
    assert !cache.containsKey(cacheKey);

    Item<M> item = itemBuilder.buildItem();

    if (!item.shouldCache()) {
      return new CacheResult<>(item.stream(), item.metadata());
    }

    if (item.sizeMaybe.isPresent()) {
      long size = item.sizeMaybe().get();
      if (size > getMaxFileSize()) {
        throw new ResponseTooLargeException(getMaxFileSize());
      }
      makeRoomFor(size);
    }

    try {
      // Download the response data.
      File responseData = downloadResponseData(cacheKey, item);
      M metadata = item.metadata();
      long size = responseData.length();
      ZonedDateTime expiry = getNow().plus(Duration.ofSeconds(item.ttl().get()));

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
          Level.WARNING, "Failure storing cache entry; will re-execute without caching: {}", e);
      // Re-issue the request since we don't know if we've consumed any of the response.
      Item<M> rerequested = itemBuilder.buildItem();
      return new CacheResult<>(rerequested.stream(), rerequested.metadata());
    }
  }

  /** Mark cache entry used and return a stream reading from the cache file. */
  private CacheResult<M> getResultForCacheEntry(String cacheKey) throws IOException {
    markCacheEntryUsed(cacheKey);
    return new CacheResult<>(
        new FileInputStream(cache.get(cacheKey).responseData), cache.get(cacheKey).metadata());
  }

  /**
   * Read the repsonse data from the remote server into the cache file. If the downloaded data is
   * over the file size limit, throw a ResponseTooLargeException.
   */
  private File downloadResponseData(String cacheKey, Item item)
      throws IOException, ResponseTooLargeException {
    File temp = File.createTempFile("LRUCache-" + cacheKey, "");
    temp.deleteOnExit();
    try {
      var inputStream = item.stream();
      try (var outputStream = new FileOutputStream(temp)) {

        // Limit the download to getMaxFileSize().
        boolean sizeOK = Stream_Utils.limitedCopy(inputStream, outputStream, getMaxFileSize());
        if (!sizeOK) {
          try {
            temp.delete();
            outputStream.close();
          } finally {
            // catch block below will delete the temp file.
            throw new ResponseTooLargeException(getMaxFileSize());
          }
        }

        return temp;
      }
    } catch (IOException e) {
      temp.delete();
      throw e;
    }
  }

  /** Mark the entry with the current time, to maintain LRU data. */
  private void markCacheEntryUsed(String cacheKey) {
    lastUsed.put(cacheKey, getNow());
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
    return maxFileSizeOverrideTestOnly.orElse(maxFileSize);
  }

  private long getMaxTotalCacheSize() {
    return maxTotalCacheSizeOverrideTestOnly.orElse(maxTotalCacheSize);
  }

  public int getNumEntries() {
    return cache.size();
  }

  public List<Long> getFileSizesTestOnly() {
    return new ArrayList<>(
        cache.values().stream().map(CacheEntry::size).collect(Collectors.toList()));
  }

  public void setNowOverrideTestOnly(ZonedDateTime nowOverride) {
    nowOverrideTestOnly = Optional.of(nowOverride);
  }

  public void clearNowOverrideTestOnly() {
    nowOverrideTestOnly = Optional.empty();
  }

  public void setMaxFileSizeOverrideTestOnly(long maxFileSizeOverrideTestOnly_) {
    assert maxFileSizeOverrideTestOnly_ <= maxFileSize;
    maxFileSizeOverrideTestOnly = Optional.of(maxFileSizeOverrideTestOnly_);
  }

  public void clearMaxFileSizeOverrideTestOnly() {
    maxFileSizeOverrideTestOnly = Optional.empty();
  }

  public void setMaxTotalCacheSizeOverrideTestOnly(long maxTotalCacheSizeOverrideTestOnly_) {
    assert maxTotalCacheSizeOverrideTestOnly_ <= maxTotalCacheSize;
    maxTotalCacheSizeOverrideTestOnly = Optional.of(maxTotalCacheSizeOverrideTestOnly_);
  }

  public void clearMaxTotalCacheSizeOverrideTestOnly() {
    maxTotalCacheSizeOverrideTestOnly = Optional.empty();
  }

  private ZonedDateTime getNow() {
    return nowOverrideTestOnly.orElse(ZonedDateTime.now());
  }

  private record CacheEntry<M>(File responseData, M metadata, long size, ZonedDateTime expiry) {}

  public record Item<M>(
      InputStream stream, M metadata, Optional<Long> sizeMaybe, Optional<Integer> ttl) {

    public boolean shouldCache() {
      return ttl.isPresent();
    }
  }

  public record CacheResult<M>(InputStream inputStream, M metadata) {}

  public interface ItemBuilder<M> {
    String makeCacheKey();

    /**
     * Returning an Item with no TTL indicates that the data should not be cached.
     */
    Item<M> buildItem() throws IOException, InterruptedException;
  }

  private final Comparator<Map.Entry<String, CacheEntry<M>>> cacheEntryLRUComparator =
      Comparator.comparing(me -> lastUsed.get(me.getKey()));
}
