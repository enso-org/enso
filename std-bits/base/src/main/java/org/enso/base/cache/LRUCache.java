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
 * <p>Limits should be set with environment variables:
 *
 * <p>
 *
 * <pre>
 * - ENSO_LIB_HTTP_CACHE_MAX_FILE_SIZE_MB: single file size, in MB
 * - ENSO_LIB_HTTP_CACHE_MAX_TOTAL_CACHE_LIMIT: total cache size, in MB or
 *   percentage of free disk space
 * </pre>
 *
 * <p>Examples:
 *
 * <pre>
 *   ENSO_LIB_HTTP_CACHE_MAX_FILE_SIZE_MB=20
 *   ENSO_LIB_HTTP_CACHE_MAX_TOTAL_CACHE_LIMIT=200 ENSO_LIB_HTTP_CACHE_MAX_TOTAL_CACHE_LIMIT=50%
 * </pre>
 *
 * <p>Regardless of other settings, the total cache size is capped at a percentage of the free disk
 * space (MAX_PERCENTAGE).
 *
 * @param <M> Additional metadata to associate with the data.
 */
public class LRUCache<M> {
  private static final Logger logger = Logger.getLogger(LRUCache.class.getName());

  /**
   * An upper limit on the total cache size. If the cache size limit specified by the other
   * parameters goes over this value, then this value is used.
   */
  private static final double MAX_PERCENTAGE = 0.9;

  /** Used to override cache parameters for testing. */
  private final Map<String, CacheEntry<M>> cache = new HashMap<>();

  private final Map<String, ZonedDateTime> lastUsed = new HashMap<>();

  /** Defines the per-file and total cache size limits. */
  private final LRUCacheSettings settings;

  /** Used to get the current time; mockable. */
  private final NowGetter nowGetter;

  /** Used to get the current free disk space; mockable. */
  private final DiskSpaceGetter diskSpaceGetter;

  /** Used to clear the cache on reload. */
  private final ReloadDetector reloadDetector = new ReloadDetector();

  public LRUCache() {
    this(LRUCacheSettings.getDefault(), new NowGetter(), new DiskSpaceGetter());
  }

  public LRUCache(LRUCacheSettings settings, NowGetter nowGetter, DiskSpaceGetter diskSpaceGetter) {
    this.settings = settings;
    this.nowGetter = nowGetter;
    this.diskSpaceGetter = diskSpaceGetter;
  }

  /**
   * IOExceptions thrown by the HTTP request are propagated; exceptions thrown while creating or
   * accessing cache files are caught, and the request is re-issued without caching.
   */
  public CacheResult<M> getResult(ItemBuilder<M> itemBuilder)
      throws IOException, InterruptedException, ResponseTooLargeException {
    clearOnReload();

    String cacheKey = itemBuilder.makeCacheKey();

    try {
      if (cache.containsKey(cacheKey)) {
        return getResultForCacheEntry(cacheKey);
      } else {
        return makeRequestAndCache(cacheKey, itemBuilder);
      }
    } catch (LRUCacheException e) {
      // Re-issue the request without caching.
      // We don't re-attempt to store the cache file and entry. In some cases
      // (such as a cache file deleted from the outside), we could, but this is
      // a rare case so it seems unnecessary.
      logger.log(
          Level.WARNING,
          "Error in cache file handling; will re-execute without caching: {0}",
          e.getMessage());
      Item<M> rerequested = itemBuilder.buildItem();
      return new CacheResult<>(rerequested.stream(), rerequested.metadata());
    }
  }

  private CacheResult<M> makeRequestAndCache(String cacheKey, ItemBuilder<M> itemBuilder)
      throws IOException, InterruptedException, LRUCacheException, ResponseTooLargeException {
    assert !cache.containsKey(cacheKey) : "Cache should not contain key " + cacheKey;

    Item<M> item = itemBuilder.buildItem();

    if (!item.shouldCache()) {
      return new CacheResult<>(item.stream(), item.metadata());
    }

    long maxAllowedDownloadSize = getMaxAllowedDownloadSize();

    // If we have a content-length, clear up enough space for that. If not,
    // then clear up enough space for the largest allowed size.
    if (item.sizeMaybe.isPresent()) {
      long size = item.sizeMaybe().get();
      if (size > maxAllowedDownloadSize) {
        throw new ResponseTooLargeException(size, maxAllowedDownloadSize);
      }
      makeRoomFor(size);
    } else {
      makeRoomFor(maxAllowedDownloadSize);
    }

    try {
      // Download the response data.
      File responseData = downloadResponseData(cacheKey, item);
      M metadata = item.metadata();
      long size = responseData.length();
      ZonedDateTime expiry = nowGetter.get().plus(Duration.ofSeconds(item.ttl().get()));

      // Create a cache entry.
      var cacheEntry = new CacheEntry<>(responseData, metadata, size, expiry);
      cache.put(cacheKey, cacheEntry);
      markCacheEntryUsed(cacheKey);

      return getResultForCacheEntry(cacheKey);
    } catch (IOException e) {
      // Throw this to re-issue the request since we don't know if we've consumed any of the
      // response.
      throw new LRUCacheException("Failure storing cache entry", e);
    }
  }

  /**
   * Mark cache entry used and return a stream reading from the cache file.
   *
   * <p>If the file has been deleted, an LRUCacheException is thrown, causing .makeRequest to
   * re-issue the request without caching.
   */
  private CacheResult<M> getResultForCacheEntry(String cacheKey)
      throws IOException, LRUCacheException {
    var cacheFile = cache.get(cacheKey).responseData;

    if (!cacheFile.exists()) {
      removeCacheEntry(cacheKey, cache.get(cacheKey));
      throw new LRUCacheException("Missing cache file " + cacheFile.getPath());
    }

    markCacheEntryUsed(cacheKey);
    return new CacheResult<>(new FileInputStream(cacheFile), cache.get(cacheKey).metadata());
  }

  /**
   * Read the repsonse data from the remote server into the cache file. If the downloaded data is
   * over the file size limit, throw a ResponseTooLargeException.
   */
  private File downloadResponseData(String cacheKey, Item item)
      throws IOException, ResponseTooLargeException {
    File temp = File.createTempFile("LRUCache-" + cacheKey, "");
    temp.deleteOnExit();
    var inputStream = item.stream();
    var outputStream = new FileOutputStream(temp);
    boolean successful = false;
    try {
      long maxAllowedDownloadSize = getMaxAllowedDownloadSize();
      boolean sizeOK = Stream_Utils.limitedCopy(inputStream, outputStream, maxAllowedDownloadSize);

      if (sizeOK) {
        successful = true;
        return temp;
      } else {
        throw new ResponseTooLargeException(null, maxAllowedDownloadSize);
      }
    } finally {
      outputStream.close();
      if (!successful) {
        if (!temp.delete()) {
          logger.log(Level.WARNING, "Unable to delete cache file (key {})", cacheKey);
        }
      }
    }
  }

  /** Mark the entry with the current time, to maintain LRU data. */
  private void markCacheEntryUsed(String cacheKey) {
    lastUsed.put(cacheKey, nowGetter.get());
  }

  /** Remove all cache entries (and their files) that have passed their TTL. */
  private void removeStaleEntries() {
    var now = nowGetter.get();
    removeCacheEntriesByPredicate(e -> e.expiry().isBefore(now));
  }

  /** Remove all cache entries (and their files). */
  public void clear() {
    removeCacheEntriesByPredicate(e -> true);
  }

  private void clearOnReload() {
    if (reloadDetector.hasReloadOccurred()) {
      clear();
    }
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
    removeCacheEntry(toRemove.getKey(), toRemove.getValue());
  }

  /** Remove a cache entry: from `cache`, `lastUsed`, and the filesystem. */
  private void removeCacheEntry(String key, CacheEntry<M> value) {
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

    // Size of files on disk.
    long currentCacheSize = getTotalCacheSize();
    // Upper limit to cache size.
    long maxTotalCacheSize = getMaxTotalCacheSize(currentCacheSize);
    // Size including new file.
    long totalSize = currentCacheSize + newFileSize;

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
    assert totalSize <= maxTotalCacheSize
        : "totalSize > maxTotalCacheSize (" + totalSize + " > " + maxTotalCacheSize + ")";
    removeCacheEntries(toRemove);
  }

  private SortedSet<Map.Entry<String, CacheEntry<M>>> getSortedEntries() {
    var sortedEntries = new TreeSet<Map.Entry<String, CacheEntry<M>>>(cacheEntryLRUComparator);
    sortedEntries.addAll(cache.entrySet());
    return sortedEntries;
  }

  private long getTotalCacheSize() {
    return cache.values().stream().collect(Collectors.summingLong(e -> e.size()));
  }

  /**
   * Calculate the max total cache size, using the current limit but also constraining the result to
   * the upper bound.
   */
  public long getMaxTotalCacheSize(long currentlyUsed) {
    long freeSpace = diskSpaceGetter.get() + currentlyUsed;

    var totalCacheSize =
        switch (settings.getTotalCacheLimit()) {
          case TotalCacheLimit.Bytes bytes -> bytes.bytes();
          case TotalCacheLimit.Percentage percentage -> {
            yield (long) (percentage.percentage() * freeSpace);
          }
        };
    long upperBound = (long) (freeSpace * MAX_PERCENTAGE);
    return Long.min(upperBound, totalCacheSize);
  }

  /**
   * Calculate the largest size we can allow, which is the minimum of the max file size and the max
   * total cache size.
   */
  private long getMaxAllowedDownloadSize() {
    return Long.min(settings.getMaxFileSize(), getMaxTotalCacheSize());
  }

  /** For testing. */
  public long getMaxTotalCacheSize() {
    return getMaxTotalCacheSize(getTotalCacheSize());
  }

  public int getNumEntries() {
    return cache.size();
  }

  /** Public for testing. */
  public List<Long> getFileSizes() {
    return new ArrayList<>(
        cache.values().stream().map(CacheEntry::size).collect(Collectors.toList()));
  }

  /** Public for testing. */
  public List<String> getFiles() {
    return new ArrayList<>(
        cache.values().stream()
            .map(CacheEntry::responseData)
            .map(f -> f.getPath())
            .collect(Collectors.toList()));
  }

  /** Public for testing. */
  public LRUCacheSettings getSettings() {
    return settings;
  }

  /** Public for testing. */
  public void simulateReloadTestOnly() {
    reloadDetector.simulateReloadTestOnly();
  }

  private record CacheEntry<M>(File responseData, M metadata, long size, ZonedDateTime expiry) {}

  /**
   * A record to define the contents and properties of something to be cached.
   *
   * @param stream The InputStream providing the contents of the thing to be cached.
   * @param sizeMaybe (Optional) The size of the data provided by the InputStream
   * @param ttl (Optional) The time for which the data is fresh. If the returned Item has a TTL of
   *     0, the item will not be cahced at all.
   */
  public record Item<M>(
      InputStream stream, M metadata, Optional<Long> sizeMaybe, Optional<Integer> ttl) {

    public boolean shouldCache() {
      return ttl.isPresent();
    }
  }

  public record CacheResult<M>(InputStream inputStream, M metadata) {}

  /** Wraps code that creates an Item to be cached. */
  public interface ItemBuilder<M> {
    /** Generate a unique key for the Item */
    String makeCacheKey();

    /**
     * Creates the Item to be cached. Returning an Item with no TTL indicates that the data should
     * not be cached. This is only called when the Item is not already present in the cache.
     */
    Item<M> buildItem() throws IOException, InterruptedException;
  }

  private final Comparator<Map.Entry<String, CacheEntry<M>>> cacheEntryLRUComparator =
      Comparator.comparing(me -> lastUsed.get(me.getKey()));

  /** Represents an internal error in creating or accessing the cache file. */
  private static class LRUCacheException extends Exception {
    public LRUCacheException(String errorMessage) {
      super(errorMessage);
    }

    public LRUCacheException(String errorMessage, Throwable cause) {
      super(errorMessage, cause);
    }

    public String getMessage() {
      var cause = getCause();
      var causeMessage = cause == null ? "" : ": " + cause.getMessage();
      return super.getMessage() + causeMessage;
    }
  }
}
