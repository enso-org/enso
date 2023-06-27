package org.enso.base;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import org.graalvm.collections.Pair;
import org.graalvm.polyglot.Value;

public class Replacer_Cache {
  private static final int lruSize = 5;

  // Circular buffer containing the most recent cache keys.
  private static final List<Pair<String, Value>> lru = new ArrayList<>(lruSize);

  static {
    for (int i = 0; i < lruSize; ++i) {
      lru.add(null);
    }
  }

  // Index into the circular buffer.
  private static int nextSlot = 0;

  public static Value get_or_set(String key, Function<Void, Value> value_producer) {
    Value value = get(key);
    if (value == null) {
      value = value_producer.apply(null);
      lru.set(nextSlot, Pair.create(key, value));
      nextSlot = (nextSlot + 1) % lruSize;
    }
    return value;
  }

  // Visible for testing.
  public static Value get(String key) {
    for (int i = 0; i < lruSize; ++i) {
      Pair<String, Value> pair = lru.get(i);
      if (pair != null && pair.getLeft().equals(key)) {
        return lru.get(i).getRight();
      }
    }
    return null;
  }

  public static int getLruSize() {
    return lruSize;
  }
}
