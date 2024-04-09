package org.enso.base.enso_cloud;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.function.Function;

public final class CloudRequestCache {
  private record CacheEntry(Object value, LocalDateTime expiresAt) {}

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
      cache.put(key, new CacheEntry(value, LocalDateTime.now().plus(ttl)));
      return value;
    }
  }
}
