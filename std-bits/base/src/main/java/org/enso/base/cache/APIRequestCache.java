package org.enso.base.cache;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.function.Function;
import org.enso.base.cache.ReloadDetector;
import org.graalvm.polyglot.Value;

/**
 * A cache that can be used to save results of requests to some API to avoid re-fetching them every
 * time.
 *
 * <p>The cache is supposed to store the already processed (parsed etc.) result, that is relatively
 * small. If the result is not cached or the cache entry is expired, the cache will recompute the
 * value using the provided callback.
 */
public class APIRequestCache {
  private final HashMap<String, CacheEntry> cache = new HashMap<>();
  private final ReloadDetector reloadDetector;

  protected APIRequestCache() {
    this(null);
  }

  protected APIRequestCache(ReloadDetector reloadDetector) {
    this.reloadDetector = reloadDetector;
  }

  public void clear() {
    cache.clear();
  }

  public Object getOrCompute(String key, Function<String, Value> compute, Duration ttl) {
    clearOnReload();

    if (ttl == null) {
      // If the TTL is null, we deliberately ignore the cache.
      return compute.apply(key);
    }

    cleanExpiredEntries();

    var entry = cache.get(key);
    if (entry != null && entry.expiresAt.isAfter(LocalDateTime.now())) {
      return entry.value;
    } else {
      var value = compute.apply(key);
      put(key, value, ttl);
      return value;
    }
  }

  public void invalidateEntry(String key) {
    clearOnReload();

    cache.remove(key);
  }

  public void invalidatePrefix(String prefix) {
    clearOnReload();

    cache.keySet().removeIf(key -> key.startsWith(prefix));
  }

  public void cleanExpiredEntries() {
    clearOnReload();

    boolean hasExpiredEntries =
        firstToExpire != null && firstToExpire.isBefore(LocalDateTime.now());
    if (hasExpiredEntries) {
      cache.entrySet().removeIf(entry -> entry.getValue().expiresAt.isBefore(LocalDateTime.now()));
      firstToExpire =
          cache.values().stream()
              .map(CacheEntry::expiresAt)
              .min(LocalDateTime::compareTo)
              .orElse(null);
    }
  }

  public void put(String key, Value value, Duration ttl) {
    clearOnReload();

    if (ttl == null) {
      // If the TTL is null, we deliberately ignore the cache.
      return;
    }

    var expiresAt = LocalDateTime.now().plus(ttl);
    if (firstToExpire == null || expiresAt.isBefore(firstToExpire)) {
      firstToExpire = expiresAt;
    }

    cache.put(key, new CacheEntry(value, expiresAt));
  }

  private void clearOnReload() {
    if (reloadDetector != null && reloadDetector.hasReloadOccurred()) {
      clear();
    }
  }

  /** Public for testing. */
  public void simulateReloadTestOnly() {
    if (reloadDetector != null) {
      reloadDetector.simulateReloadTestOnly();
    }
  }

  /** Public for testing. */
  public boolean isCachedTestOnly(String key) {
    return cache.containsKey(key);
  }

  private LocalDateTime firstToExpire = null;

  private record CacheEntry(Value value, LocalDateTime expiresAt) {}
}
