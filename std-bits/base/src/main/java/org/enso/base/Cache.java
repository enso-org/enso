package org.enso.base;

import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

public abstract class Cache<Key, Value> {
  protected static final int DEFAULT_LRU_SIZE = 5;
  protected final int lruSize;

  // Circular buffer containing the most recent cache keys.
  private final List<Map.Entry<Key, Value>> lru;

  protected Cache(int lruSize) {
    this.lruSize = lruSize;
    lru = new ArrayList<>(lruSize);
    for (int i = 0; i < lruSize; ++i) {
      lru.add(null);
    }
  }

  // Index into the circular buffer.
  private int nextSlot = 0;

  public Value get_or_set(Key key, Function<Void, Value> value_producer) {
    Value value = get(key);
    if (value == null) {
      value = value_producer.apply(null);
      lru.set(nextSlot, new AbstractMap.SimpleEntry<>(key, value));
      nextSlot = (nextSlot + 1) % lruSize;
    }
    return value;
  }

  // Visible for testing.
  public Value get(Key key) {
    for (int i = 0; i < lruSize; ++i) {
      var pair = lru.get(i);
      if (pair != null && pair.getKey().equals(key)) {
        return lru.get(i).getValue();
      }
    }
    return null;
  }

  public int getLruSize() {
    return lruSize;
  }
}
