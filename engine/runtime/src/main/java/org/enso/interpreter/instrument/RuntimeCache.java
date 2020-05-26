package org.enso.interpreter.instrument;

import java.lang.ref.SoftReference;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

/** A storage for computed values. */
public class RuntimeCache {

  private final Map<UUID, SoftReference<Object>> cache = new HashMap<>();

  /** Add computed value to the cache. */
  public Object put(UUID key, Object value) {
    return cache.put(key, new SoftReference<>(value));
  }

  /** Get value from the cache. */
  public Object get(UUID key) {
    SoftReference<Object> ref = cache.get(key);
    return ref != null ? ref.get() : null;
  }

  /** Remove value from the cache. */
  public Object remove(UUID key) {
    SoftReference<Object> ref = cache.remove(key);
    return ref == null ? null : ref.get();
  }

  /** @return all cache keys. */
  public Set<UUID> getKeys() {
    return cache.keySet();
  }

  /** Clear the cache. */
  public void clear() {
    cache.clear();
  }
}
