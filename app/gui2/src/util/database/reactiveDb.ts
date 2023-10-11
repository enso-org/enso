import { ObservableV2 } from 'lib0/observable'
import { reactive } from 'vue'

export class ReactiveDb<Key, Value> extends ObservableV2<[Key, Value]> {
  internal: Map<Key, Value>
  constructor() {
	super()
	this.internal = new Map()
  }
  set(key: Key, value: Value) {
	this.internal.set(key, value)
	this.emit('keyadded', [key, value])
  }
  get(key: Key): Value | undefined {
	return this.internal.get(key)
  }
  delete(key: Key): boolean {
	return this.internal.delete(key)
  }
  get size() {
	return this.internal.size
  }
}

export class ReactiveIndex<IndexKey, IndexValue> {
  straight: Map<IndexKey, Set<IndexValue>>
  reverse: Map<IndexValue, IndexKey>
  constructor(db: ReactiveDb, indexer, reverseIndexer) {
	this.straight = reactive(new Map())
	this.reverse = reactive(new Map())
	db.on('keyadded', (key, value) => {
	  indexer(key, value, this.straight)
	  reverseIndexer(key, value, this.reverse)
	})
  }
  lookup(key: IndexKey): Set<IndexValue> | undefined {
	return this.straight.get(key)
  }
  reverseLookup(value: IndexValue): Set<IndexKey> | undefined {
	return this.reverse.get(value)
  }
}
