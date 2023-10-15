/*
   Reactive-friendly key-value database.

   `ReactiveDb` is a database itself, a thin adapter around ordinary `Map`.

   `ReactiveIndex` is an abstraction for building arbitrary database indexing with reactive updates.
*/

import { setIfUndefined } from 'lib0/map'
import { ObservableV2 } from 'lib0/observable'
import { reactive, watch } from 'vue'

export type CleanupFn = (() => void) | null
export type OnCleanup = (cleanupFn: CleanupFn) => void

export class ReactiveDb<Key, Value> extends ObservableV2<{
  entryAdded(key: Key, value: Value, onCleanup: OnCleanup): void
}> {
  internal: Map<Key, Value>
  onCleanup: Map<Key, CleanupFn>
  constructor() {
    super()
    this.internal = new Map()
    this.onCleanup = new Map()
  }
  /* Same as `Map.set`. Also emits `entryAdded` event. */
  set(key: Key, value: Value) {
    if (this.internal.has(key)) {
      this.delete(key)
    }
    this.internal.set(key, value)
    const onCleanup: OnCleanup = (callback) => {
      this.onCleanup.set(key, callback)
    }
    this.emit('entryAdded', [key, value, onCleanup])
  }
  /* Same as `Map.get` */
  get(key: Key): Value | undefined {
    return this.internal.get(key)
  }
  /* Same as `Map.delete` */
  delete(key: Key): boolean {
    const callback = this.onCleanup.get(key)
    if (callback) {
      callback()
      this.onCleanup.delete(key)
    }
    return this.internal.delete(key)
  }
  /* Same as `Map.size` */
  get size(): number {
    return this.internal.size
  }
}

export type IndexFn<IndexKey, IndexValue> = (key: IndexKey, value: IndexValue) => void
export type Indexer<Key, Value, IndexKey, IndexValue> = (
  key: Key,
  value: Value,
  index: IndexFn<IndexKey, IndexValue>,
  onDelete: OnCleanup,
) => void

/* Automatic indexing facility for `ReactiveDb` */
export class ReactiveIndex<Key, Value, IndexKey, IndexValue> {
  forward: Map<IndexKey, Set<IndexValue>>
  reverse: Map<IndexValue, Set<IndexKey>>
  indexer: Indexer | null
  constructor(db: ReactiveDb<Key, Value>, indexer: Indexer<Key, Value, IndexKey, IndexValue>) {
    this.forward = reactive(new Map())
    this.reverse = reactive(new Map())
	this.indexer = indexer
    db.on('entryAdded', (key, value, onDelete) => {
      const keyValues = indexer(key, value)
	  const stop = watch(() => indexer(key, value),
						 (keyValues, _, cleanup) => {
						   keyValues.forEach((value, key) => {
							 this.writeToIndex(key, value)
						   })
						   cleanup(() => keyValues.forEach((value, key) => this.removeFromIndex(key, value)))
						 }, { immediate: true })
	  onDelete(() => stop())
    })
  }
  updateEntryIndex(closure: () => Map<IndexKey, IndexValue>) {
	const stop = watch(closure, (keyValues, _, onCleanup) => {
	  keyValues.forEach((value, key) => {
		this.writeToIndex(key, value)
	  })
	  onCleanup(() => {
		keyValues.forEach((value, key) => this.removeFromIndex(key, value))
		stop()
		invalidatedClosures.add(closure)
	  })

	  
	}, { immediate: true, flush: 'sync' })
  }
  flush() {
	runAllInvalidatedClosures()
  }
  /* Internal method for adding a new key-value relation to the index. */
  writeToIndex(key: IndexKey, value: IndexValue): void {
    const forward = setIfUndefined(this.forward, key, () => new Set())
    forward.add(value)
    const reverse = setIfUndefined(this.reverse, value, () => new Set())
    reverse.add(key)
  }
  /* Internal method for removing a certain key-value relation from the index */
  removeFromIndex(key: IndexKey, value: IndexValue): void {
    // @ts-ignore
    const remove = (map, key, value) => {
      map.get(key)?.delete(value)
    }
    remove(this.forward, key, value)
    remove(this.reverse, value, key)
  }
  /* Look for key in the forward index. */
  lookup(key: IndexKey): Set<IndexValue> {
    return this.forward.get(key) ?? new Set()
  }
  /* Look for value in the reverse index, returning a set of all keys with this value. */
  reverseLookup(value: IndexValue): Set<IndexKey> {
    return this.reverse.get(value) ?? new Set()
  }
}
