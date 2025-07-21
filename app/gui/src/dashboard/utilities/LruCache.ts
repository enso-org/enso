/** @file A simple LRU cache. */

import { assert } from '@/util/assert'

/** Anything except specifically `undefined`. */
// eslint-disable-next-line @typescript-eslint/no-empty-object-type
type NotUndefined = {} | null

/**
 * A simple LRU cache.
 *
 *
 * Implementation based on https://github.com/dominictarr/hashlru#algorithm
 *
 * Note: `V` must not be `undefined`, since the implementation assumes that an `undefined` value
 * returned by `Map.get(k)` method indicates that the key `k` was not present in the map.
 */
export class LRUCache<K, V extends NotUndefined> {
  /**
   * A table of at most N recently added values.
   */
  private recentGeneration: Map<K, V>
  /**
   * A table of older values that will be removed next time the cache size limit is reached.
   * This map is internally guaranteed to NOT contain any keys present in `recentCache` at the time.
   */
  private oldGeneration: Map<K, V>

  /**
   * Create a new LRU cache.
   *
   * At any time, the cache will contain between `minimumCapacity` and `minimumCapacity*2` items,
   * and it will guarantee that the `minimumCapacity` of least recently written or accessed items
   * will be resolvable.
   *
   * Note: `minimumCapacity` must be greater than 0.
   */
  constructor(
    private readonly minimumCapacity: number,
    private readonly onEvict?: (value: V, key: K) => void,
  ) {
    assert(minimumCapacity > 0)
    this.recentGeneration = new Map()
    this.oldGeneration = new Map()
  }

  /**
   * Take a value from the cache and assume ownership, i.e. remove it from cache without evicting and return it.
   */
  take(key: K): V | undefined {
    const recent = this.tryTake(this.recentGeneration, key)
    if (recent !== undefined) return recent
    return this.tryTake(this.oldGeneration, key)
  }

  /**
   * Get a value from the cache, mark it as "recently used".
   */
  get(key: K): V | undefined {
    const recentValue = this.recentGeneration.get(key)
    if (recentValue !== undefined) return recentValue

    // No recent value found, attempt recovering one from old generation.
    const oldValue = this.tryTake(this.oldGeneration, key)
    if (oldValue !== undefined) this.recentGeneration.set(key, oldValue)
    return oldValue
  }

  /**
   * Get a value from the cache, but do NOT mark it as "recently used".
   * The peeked value continues to be eligible for cleanup in any potential next `set` call.
   */
  peek(key: K): V | undefined {
    const recentValue = this.recentGeneration.get(key)
    if (recentValue !== undefined) return recentValue
    return this.oldGeneration.get(key)
  }

  /**
   * Set a value in the cache and mark it as "recently used".
   */
  set(key: K, value: V) {
    const prevValue = this.recentGeneration.get(key)
    this.recentGeneration.set(key, value)

    if (prevValue === undefined) {
      // We have just inserted new key into the `recentCache`, uphold the guarantee of old/new tables not having common keys.
      const oldValue = this.tryTake(this.oldGeneration, key)
      if (oldValue !== undefined && this.onEvict) this.onEvict(oldValue, key)
      this.evictIfNecessary()
    } else if (this.onEvict) this.onEvict(prevValue, key)
  }

  /**
   * Clear the cache.
   */
  clear() {
    this.callEvictHandler(this.oldGeneration)
    this.oldGeneration.clear()
    this.callEvictHandler(this.recentGeneration)
    this.recentGeneration.clear()
  }

  /**
   * Evict the oldest values from the cache and swap buffers.
   */
  private evictIfNecessary() {
    if (this.recentGeneration.size >= this.minimumCapacity) {
      this.callEvictHandler(this.oldGeneration)
      this.oldGeneration.clear()
      ;[this.oldGeneration, this.recentGeneration] = [this.recentGeneration, this.oldGeneration]
    }
  }

  /**
   * Call evict handler for given cache table, if present.
   */
  private callEvictHandler(table: Map<K, V>) {
    const onEvict = this.onEvict
    if (onEvict) table.forEach((k, v) => onEvict(k, v))
  }

  /**
   * Try moving a value out of given table. Returns `undefined` if value was not present.
   */
  private tryTake(table: Map<K, V>, key: K) {
    const value = table.get(key)
    if (value !== undefined) table.delete(key)
    return value
  }
}
