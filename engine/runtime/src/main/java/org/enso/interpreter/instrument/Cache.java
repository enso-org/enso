package org.enso.interpreter.instrument;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

/**
 * A storage for computed values.
 */
public class Cache {

  private final Map<UUID, Object> cache = new HashMap<>();

  /** Add computed value to the cache. */
  public Object put(UUID key, Object value) {
    return cache.put(key, value);
  }

  /** Get value from the cache. */
  public Object get(UUID key) {
    return cache.get(key);
  }
}
