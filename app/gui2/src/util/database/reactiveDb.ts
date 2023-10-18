/**
 * Tools for managing reactive key-value databases.
 *
 * 1. `ReactiveDb`: This is a reactivity-friendly database that serves as a thin adapter around a standard `Map`.
 *    It offers the basic functionality of a `Map` while also emitting events that allow observers to track changes efficiently.
 *
 * 2. `ReactiveIndex`: This is an abstraction, that allows constructing arbitrary database indexes.
 *    Indexes update reactively, can depend on each other, and defer updates until the next lookup for efficiency.
 */

import { LazySyncEffectSet } from '@/util/reactivity'
import { setIfUndefined } from 'lib0/map'
import { ObservableV2 } from 'lib0/observable'
import { reactive } from 'vue'

export type OnDelete = (cleanupFn: () => void) => void

/**
 * Represents a reactive database adapter that extends the behaviors of `Map`.
 * It emits an `entryAdded` event when a new entry is added to the database,
 * facilitating reactive tracking of database insertions and deletions.
 *
 * @typeParam K - The key type for the database entries.
 * @typeParam V - The value type for the database entries.
 */
export class ReactiveDb<K, V> extends ObservableV2<{
  entryAdded(key: K, value: V, onDelete: OnDelete): void
}> {
  internal: Map<K, V>
  onDelete: Map<K, Set<() => void>>

  constructor() {
    super()
    this.internal = new Map()
    this.onDelete = new Map()
  }

  /**
   * Sets a new key-value pair in the database, equivalent to `Map.set`.
   * The function also emits an `entryAdded` event after the addition.
   *
   * @param key - The key for the new entry.
   * @param value - The value for the new entry.
   */
  set(key: K, value: V) {
    // Trigger a reactive update when replacing one entry with another.
    this.delete(key)

    this.internal.set(key, value)
    const onDelete: OnDelete = (callback) => {
      const callbacks = setIfUndefined(this.onDelete, key, () => new Set())
      callbacks.add(callback)
    }
    this.emit('entryAdded', [key, value, onDelete])
  }

  /**
   * Retrieves the value corresponding to a specified key in the database, equivalent to `Map.get`.
   *
   * @param key - The key for which to retrieve the value.
   * @returns The value associated with the key, or undefined if the key is not found.
   */
  /** Same as `Map.get` */
  get(key: K): V | undefined {
    return this.internal.get(key)
  }

  /**
   * Deletes the key-value pair identified by the specified key from the database, equivalent to `Map.delete`.
   *
   * @param key - The key for which to delete the corresponding key-value pair.
   * @returns True if the deletion was successful, or false if the key was not found.
   */
  delete(key: K): boolean {
    this.onDelete.get(key)?.forEach((callback) => callback())
    this.onDelete.delete(key)
    return this.internal.delete(key)
  }

  /**
   * Retrieves the number of key-value pairs currently in the database, equivalent to `Map.size`.
   *
   * @returns The number of key-value pairs in the database.
   */
  get size(): number {
    return this.internal.size
  }

  /**
   * Retrieves an iterator over entries in the database, equivalent to `Map.entries`.
   *
   * @returns An iterator that yields key-value pairs in the database.
   */
  entries(): IterableIterator<[K, V]> {
    return this.internal.entries()
  }
}

/**
 * A function type representing an indexer for a `ReactiveDb`.
 *
 * An `Indexer` takes a key-value pair from the `ReactiveDb` and produces an array of index key-value pairs,
 * defining how the input key and value maps to keys and values in the index.
 *
 * @param key - The key from the `ReactiveDb`.
 * @param value - The value from the `ReactiveDb`.
 *
 * @returns An Array representing multiple key-value pair(s) ([IK, IV]) for the index.
 */
export type Indexer<K, V, IK, IV> = (key: K, value: V) => [IK, IV][]

/**
 * Provides automatic indexing for a `ReactiveDb` instance.
 * Utilizes both forward and reverse mapping for efficient lookups and reverse lookups.
 *
 * @typeParam K - The key type of the ReactiveDb.
 * @typeParam V - The value type of the ReactiveDb.
 * @typeParam IK - The key type of the index.
 * @typeParam IV - The value type of the index.
 */
export class ReactiveIndex<K, V, IK, IV> {
  /** Forward map from index keys to a set of index values */
  forward: Map<IK, Set<IV>>
  /** Reverse map from index values to a set of index keys */
  reverse: Map<IV, Set<IK>>
  /** Collections of effects to sync data between ReactiveDB and ReactiveIndex */
  effects: LazySyncEffectSet

  /**
   * Constructs a new ReactiveIndex for the given ReactiveDb and an indexer function.
   *
   * @param db - The ReactiveDb to index.
   * @param indexer - The indexer function defining how db keys and values map to index keys and values.
   */
  constructor(db: ReactiveDb<K, V>, indexer: Indexer<K, V, IK, IV>) {
    this.forward = reactive(new Map())
    this.reverse = reactive(new Map())
    this.effects = new LazySyncEffectSet()
    db.on('entryAdded', (key, value, onDelete) => {
      const stopEffect = this.effects.lazyEffect((onCleanup) => {
        const keyValues = indexer(key, value)
        keyValues.forEach(([key, value]) => this.writeToIndex(key, value))
        onCleanup(() => keyValues.forEach(([key, value]) => this.removeFromIndex(key, value)))
      })
      onDelete(() => stopEffect())
    })
  }

  /**
   * Adds a new key-value pair to the forward and reverse index.
   *
   * @param key - The key to add to the index.
   * @param value - The value to associate with the key.
   */
  writeToIndex(key: IK, value: IV): void {
    const forward = setIfUndefined(this.forward, key, () => new Set())
    if (forward.has(value)) {
      console.error(
        `Attempt to repeatedly write the same key-value pair (${[
          key,
          value,
        ]}) to the index. Please check your indexer implementation.`,
      )
    }
    forward.add(value)
    const reverse = setIfUndefined(this.reverse, value, () => new Set())
    reverse.add(key)
  }

  /**
   * Removes a key-value pair from the forward and reverse index.
   *
   * @param key - The key to remove from the index.
   * @param value - The value associated with the key.
   */
  removeFromIndex(key: IK, value: IV): void {
    const remove = <K, V>(map: Map<K, Set<V>>, key: K, value: V) => {
      map.get(key)?.delete(value)
    }
    remove(this.forward, key, value)
    remove(this.reverse, value, key)
  }

  /** Look for key in the forward index.
   * Returns a set of values associated with the given index key.
   *
   * @param key - The index key to look up values for.
   * @return A set of values corresponding to the key or an empty set if the key is not present in the index.
   */
  lookup(key: IK): Set<IV> {
    this.effects.flush()
    return this.forward.get(key) ?? new Set()
  }

  /**
   * Returns a set of keys associated with the given index value.
   *
   * @param value - The index value to reverse look up keys for.
   * @return A set of keys corresponding to the value or an empty set if the value is not present in the index.
   */
  reverseLookup(value: IV): Set<IK> {
    this.effects.flush()
    return this.reverse.get(value) ?? new Set()
  }
}
