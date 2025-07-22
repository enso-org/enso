package org.enso.base.cache;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.function.Function;
import org.graalvm.polyglot.Value;

/**
 * A cache that can be used to save results of requests to some API to avoid re-fetching them every
 * time.
 *
 * <p>The cache is supposed to store the already processed (parsed etc.) result, that is relatively
 * small. If the result is not cached or the cache entry is expired, the cache will recompute the
 * value using the provided callback.
 *
 * <p>Subclasses of APIRequestCache that want to be cleared on reload should call
 * ReloadDetector.register(this).
 */
public class APIRequestCache implements ReloadDetector.HasClearableCache {
  private final HashMap<String, CacheEntry> cache = new HashMap<>();

  public void clear() {
    cache.clear();
  }

  public Object getOrCompute(String key, Function<String, Value> compute, Duration ttl) {
    ReloadDetector.clearOnReloadIfRegistered(this);

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
    ReloadDetector.clearOnReloadIfRegistered(this);

    cache.remove(key);
  }

  public void invalidatePrefix(String prefix) {
    ReloadDetector.clearOnReloadIfRegistered(this);

    cache.keySet().removeIf(key -> key.startsWith(prefix));
  }

  public void cleanExpiredEntries() {
    ReloadDetector.clearOnReloadIfRegistered(this);

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
    ReloadDetector.clearOnReloadIfRegistered(this);

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

  @Override /* HasClearableCache */
  public void clearCache() {
    cache.clear();
  }

  /** Public for testing. */
  public boolean isCachedTestOnly(String key) {
    return cache.containsKey(key);
  }

  private LocalDateTime firstToExpire = null;

  private record CacheEntry(Value value, LocalDateTime expiresAt) {}
}
