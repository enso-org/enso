package org.enso.base.enso_cloud;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.function.Function;

/**
 * A cache that can be used to save results of cloud requests to avoid re-fetching them every time.
 *
 * <p>The cache is supposed to store the already processed (parsed etc.) result. If the result is
 * not cached or the cache entry is expired, the cache will recompute the value using the provided
 * callback.
 */
public final class CloudRequestCache {
  private static final HashMap<String, CacheEntry> cache = new HashMap<>();

  public static void clear() {
    cache.clear();
  }

  public static Object getOrCompute(String key, Function<String, Object> compute, Duration ttl) {
    if (ttl == null) {
      // If the TTL is null, we deliberately ignore the cache.
      return compute.apply(key);
    }

    var entry = cache.get(key);
    if (entry != null && entry.expiresAt.isAfter(LocalDateTime.now())) {
      return entry.value;
    } else {
      var value = compute.apply(key);
      put(key, value, ttl);
      return value;
    }
  }

  public static void invalidateEntry(String key) {
    cache.remove(key);
  }

  public static void invalidatePrefix(String prefix) {
    cache.keySet().removeIf(key -> key.startsWith(prefix));
  }

  public static void put(String key, Object value, Duration ttl) {
    if (ttl == null) {
      // If the TTL is null, we deliberately ignore the cache.
      return;
    }

    cache.put(key, new CacheEntry(value, LocalDateTime.now().plus(ttl)));
  }

  private record CacheEntry(Object value, LocalDateTime expiresAt) {}
}
