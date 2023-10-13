/*
   Reactive-friendly key-value database.

   `ReactiveDb` is a database itself, a thin adapter around ordinary `Map`.

   `ReactiveIndex` is an abstraction for building arbitrary database indexing with reactive updates.
*/

import { ObservableV2 } from 'lib0/observable'
import { reactive } from 'vue'
import { setIfUndefined } from 'lib0/map'

export type CleanupFn = () => void
export type OnCleanup = (cleanupFn: CleanupFn) => void

export class ReactiveDb<Key, Value> extends ObservableV2<{ entryAdded(key: Key, value: Value, onCleanup: OnCleanup): void }> {
  internal: Map<Key, Value>
  onCleanup: Map<Key, CleanupFn>
  constructor() {
	super()
	this.internal = new Map()
	this.onCleanup = new Map()
  }
  set(key: Key, value: Value) {
	if (this.internal.has(key)) {
	  this.delete(key)
	}
	this.internal.set(key, value)
	const onCleanup = (callback) => {
	  this.onCleanup.set(key, callback)
	}
	this.emit('entryAdded', [key, value, onCleanup])
  }
  get(key: Key): Value | undefined {
	return this.internal.get(key)
  }
  delete(key: Key): boolean {
	const callback = this.onCleanup.get(key)
	if (callback) {
	  callback()
	  this.onCleanup.delete(key)
	}
	return this.internal.delete(key)
  }
  get size(): number {
	return this.internal.size
  }
}

export type IndexFn<IndexKey, IndexValue> = (key: IndexKey, value: IndexValue)
export type Indexer<Key, Value, IndexKey, IndexValue> = (key: Key, value: Value, index: IndexFn<IndexKey, IndexValue>, onDelete: OnCleanup): void

export class ReactiveIndex<Db, IndexKey, IndexValue> {
  straight: Map<IndexKey, Set<IndexValue>>
  reverse: Map<IndexValue, Set<IndexKey>>
  constructor(db: ReactiveDb<Key, Value>, indexer: Indexer<Key, Value>) {
	this.straight = reactive(new Map())
	this.reverse = reactive(new Map())
	db.on('entryAdded', (key, value, onDelete) => {
	  let indexOnDeleteCb = null
	  const index = (key, value) => {
		this.writeToIndex(key, value)
		onDelete(() => {
		  if (indexOnDeleteCb) indexOnDeleteCb()
		  this.removeFromIndex(key, value)
		})
	  }
	  const userOnDelete = (callback) => { indexOnDeleteCb = callback }
	  indexer(key, value, index, userOnDelete)
	})
  }
  writeToIndex(key: IndexKey, value: IndexValue): void {
	const straight = setIfUndefined(this.straight, key, () => new Set())
	straight.add(value)
	const reverse = setIfUndefined(this.reverse, value, () => new Set())
	reverse.add(key)
  }
  removeFromIndex(key: IndexKey, value: IndexValue): void {
	const remove = (map, key, value) => {
	  map.get(key)?.delete(value)
	}
	remove(this.straight, key, value)
	remove(this.reverse, value, key)
  }
  lookup(key: IndexKey): Set<IndexValue> {
	return this.straight.get(key) ?? new Set()
  }
  reverseLookup(value: IndexValue): Set<IndexKey> {
	return this.reverse.get(value) ?? new Set()
  }
}
