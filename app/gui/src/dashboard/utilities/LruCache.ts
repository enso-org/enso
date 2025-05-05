/** @file A simple LRU cache. */
/**
 * A simple LRU cache.
 *
 * Implementation based on https://github.com/dominictarr/hashlru#algorithm
 */
export class LRUCache<K, V> {
  private oldCache: Map<K, V>
  private cache: Map<K, V>

  /**
   * Create a new LRU cache.
   */
  constructor(private readonly maxSize: number) {
    this.cache = new Map()
    this.oldCache = new Map()
  }

  /**
   * Get a value from the cache.
   */
  get(key: K): V | undefined {
    const newCacheValue = this.cache.get(key)

    if (newCacheValue != null) {
      return newCacheValue
    }

    const oldCacheValue = this.oldCache.get(key)

    if (oldCacheValue != null) {
      this.cache.set(key, oldCacheValue)
      this.evictIfNecessary()
    }

    return oldCacheValue
  }

  /**
   * Set a value in the cache.
   */
  set(key: K, value: V) {
    const isValueInNewCache = this.cache.has(key)

    this.cache.set(key, value)

    if (isValueInNewCache) {
      this.evictIfNecessary()
    }
  }

  /**
   * Clear the cache.
   */
  clear() {
    this.cache.clear()
    this.oldCache.clear()
  }

  /**
   * Evict the oldest value from the cache.
   */
  private evictIfNecessary() {
    if (this.cache.size > this.maxSize) {
      this.oldCache = this.cache
      this.cache = new Map()
    }
  }
}
