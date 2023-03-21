package org.enso.base;

import org.graalvm.collections.Pair;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class Replacer_Cache {
  private static final int lruSize = 5;

  // Circular buffer containing the most recent cache keys.
  private static final ArrayList<Pair<String, Object>> lru = new ArrayList<>(lruSize);

  static {
    for (int i=0; i < lruSize; ++i) {
      lru.add(null);
    }
  }

  // Index into the circular buffer.
  private static int nextSlot = 0;

  public static void put(String key, Object value) {
    lru.set(nextSlot, Pair.create(key, value));
    nextSlot = (nextSlot + 1) % lruSize;
  }

  public static Object get(String key) {
    for (int i=0; i < lruSize; ++i) {
      Pair<String, Object> pair = lru.get(i);
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
