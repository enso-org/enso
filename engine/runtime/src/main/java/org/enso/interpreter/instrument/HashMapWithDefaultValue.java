package org.enso.interpreter.instrument;

import java.util.HashMap;

public class HashMapWithDefaultValue<K, V> extends HashMap<K, V> {

  protected V defaultValue;

  public HashMapWithDefaultValue(V defaultValue) {
    this.defaultValue = defaultValue;
  }

  @Override
  public V get(Object key) {
    return containsKey(key) ? super.get(key) : defaultValue;
  }
}
