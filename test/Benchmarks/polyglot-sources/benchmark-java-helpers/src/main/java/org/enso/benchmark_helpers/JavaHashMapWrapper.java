package org.enso.benchmark_helpers;

import java.util.HashMap;

/**
 * Wraps a Java HashMap into an interface hiding it, to ensure that we are calling the raw HashMap and are not using the
 * Enso conversions that may be applied automatically. This allows us to compare the raw HashMap performance with other
 * variants.
 */
public class JavaHashMapWrapper {
  private final HashMap<Object, Object> map = new HashMap<>();

  public JavaHashMapWrapper insert(Object key, Object value) {
    map.put(key, value);
    return this;
  }

  public Object get(Object key) {
    return map.get(key);
  }
}
