package org.enso.microsoft.azure;

/**
 * A simple LRU cache implementation.
 *
 * @param <K> the type of keys
 * @param <V> the type of values
 */
final class LRUCache<K, V> extends java.util.LinkedHashMap<K, V> {
  private final int maxSize;

  /**
   * Constructs an LRUCache with the specified maximum size.
   *
   * @param maxSize the maximum size of the cache
   */
  public LRUCache(int maxSize) {
    super(maxSize, 0.75f, true);
    this.maxSize = maxSize;
  }

  @Override
  protected boolean removeEldestEntry(java.util.Map.Entry<K, V> eldest) {
    return size() > maxSize;
  }
}
