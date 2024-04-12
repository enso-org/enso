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
    if (cache.remove(key) != null) {
      System.out.println("Invalidated cache entry for key: " + key);
    }
  }

  public static void invalidatePrefix(String prefix) {
    long cnt = cache.keySet().stream().filter(key -> key.startsWith(prefix)).count();
    if (cnt > 0) {
      System.out.println("Invalidated " + cnt + " cache entries with prefix: " + prefix);
    }
    cache.keySet().removeIf(key -> key.startsWith(prefix));
  }

  public static void put(String key, Object value, Duration ttl) {
    cache.put(key, new CacheEntry(value, LocalDateTime.now().plus(ttl)));
  }

  private record CacheEntry(Object value, LocalDateTime expiresAt) {}
}
