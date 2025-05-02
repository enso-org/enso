/** @file A simple LRU cache. */
/**
 * A simple LRU cache.
 */
export class LRUCache<K, V> {
  private readonly cache: Map<K, V>
  private readonly maxSize: number

  /**
   * Create a new LRU cache.
   */
  constructor(maxSize: number) {
    this.cache = new Map()
    this.maxSize = maxSize
  }

  /**
   * Get a value from the cache.
   */
  get(key: K): V | undefined {
    const value = this.cache.get(key)
    if (value != null) {
      this.cache.delete(key)
      this.cache.set(key, value)
    }
    return value
  }

  /**
   * Set a value in the cache.
   */
  set(key: K, value: V) {
    if (this.cache.size >= this.maxSize) {
      const oldestKey = this.cache.keys().next().value
      if (oldestKey != null) {
        this.cache.delete(oldestKey)
      }
    }
    this.cache.set(key, value)
  }

  /**
   * Clear the cache.
   */
  clear() {
    this.cache.clear()
  }
}
