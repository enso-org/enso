import { ReactiveDb, ReactiveIndex } from '@/util/database/reactiveDb'
import { expect, test, vi } from 'vitest'
import { computed, nextTick, reactive } from 'vue'

test('Basic add/remove', () => {
  const db = new ReactiveDb()
  expect(db.size).toEqual(0)
  db.set('Key 1', 10)
  db.set('Key 2', 20)
  expect(db.size).toEqual(2)
  expect(db.get('Key 1')).toStrictEqual(10)
  expect(db.get('Key 2')).toStrictEqual(20)
  db.delete('Key 1')
  expect(db.size).toEqual(1)
  expect(db.get('Key 1')).toBeUndefined()
})

test('Indexing does not cause spurious reactive updates', async () => {
  const db = new ReactiveDb<number, { name: string }>()
  const map = reactive(new Map<string, string>())
  map.set('Key 1', 'v1')
  map.set('Key 2', 'v1')
  map.set('Key 3', 'v3')
  const index = new ReactiveIndex(db, (id, entry) => {
    const mappedName = map.get(entry.name)
    return mappedName ? [[mappedName, id]] : []
  })
  const indexWriteSpy = vi.spyOn(index, 'writeToIndex')
  const allKeysInMap = computed(() => [...index.allForward()].map(([k, _]) => k))

  // Adding some values to the DB, resulting in index writes.
  db.set(10, { name: 'Key 1' })
  db.set(20, { name: 'Key 1' })
  db.set(30, { name: 'Key 3' })
  // Index is flushed only when any lookup function is called.
  index.allForward()
  // 3 index writes for the 3 entries in the DB.
  expect(indexWriteSpy).toHaveBeenCalledTimes(3)
  expect(indexWriteSpy).toHaveBeenNthCalledWith(1, 'v1', 10)
  expect(indexWriteSpy).toHaveBeenNthCalledWith(2, 'v1', 20)
  expect(indexWriteSpy).toHaveBeenNthCalledWith(3, 'v3', 30)
  expect(allKeysInMap.value).toEqual(['v1', 'v3'])

  // Changing the reactive mapping, we trigger changes in the index.
  map.set('Key 1', 'v2')
  // Updates are scheduled for the next tick.
  expect(indexWriteSpy).toHaveBeenCalledTimes(3)
  await nextTick()
  // 2 more index writes for the 2 updated entries.
  expect(indexWriteSpy).toHaveBeenCalledTimes(5)
  expect(indexWriteSpy).toHaveBeenNthCalledWith(4, 'v2', 10)
  expect(indexWriteSpy).toHaveBeenNthCalledWith(5, 'v2', 20)
  // Important: we check that no spurious updates were triggered,
  // e.g. we’re not getting into an infinite loop of updates.
  expect(indexWriteSpy).toHaveBeenCalledTimes(5)
  // The computed value depending on the index is updated correctly.
  expect(allKeysInMap.value).toEqual(['v3', 'v2'])
  // Update of the mapping causes index to recalculate…
  map.delete('Key 3')
  // … but it is scheduled for the next tick.
  await nextTick()
  // Important: deleting a key triggers update of dependent reactive values.
  expect(allKeysInMap.value).toEqual(['v2'])
  // Finally, no spurious updates.
  expect(indexWriteSpy).toHaveBeenCalledTimes(5)
})

test('Indexing is efficient', () => {
  const db = new ReactiveDb<number, { name: string }>()
  const index = new ReactiveIndex(db, (id, entry) => [[entry.name, id]])
  const adding = vi.spyOn(index, 'writeToIndex')
  const removing = vi.spyOn(index, 'removeFromIndex')
  db.set(1, reactive({ name: 'abc' }))
  db.set(2, reactive({ name: 'xyz' }))
  db.set(3, reactive({ name: 'abc' }))
  db.delete(2)
  expect(adding).toHaveBeenCalledTimes(0)
  expect(removing).toHaveBeenCalledTimes(0)
  index.lookup('x')
  expect(adding).toHaveBeenCalledTimes(2)
  expect(removing).toHaveBeenCalledTimes(0)
  db.set(1, { name: 'qdr' })
  index.lookup('x')
  expect(adding).toHaveBeenCalledTimes(3)
  expect(removing).toHaveBeenCalledTimes(1)
  db.get(3)!.name = 'xyz'
  index.lookup('x')
  expect(adding).toHaveBeenCalledTimes(4)
  expect(removing).toHaveBeenCalledTimes(2)
  expect(index.lookup('qdr')).toEqual(new Set([1]))
  expect(index.lookup('xyz')).toEqual(new Set([3]))
})

test('Error reported when indexer implementation returns non-unique pairs', () => {
  const db = new ReactiveDb()
  console.error = () => {}
  const consoleError = vi.spyOn(console, 'error').mockImplementation(() => {})
  // Invalid index
  const index = new ReactiveIndex(db, (_id, _entry) => [[1, 1]])
  db.set(1, 1)
  db.set(2, 2)
  index.lookup(1)
  expect(consoleError).toHaveBeenCalledOnce()
  expect(consoleError).toHaveBeenCalledWith(
    'Attempt to repeatedly write the same key-value pair (1,1) to the index. Please check your indexer implementation.',
  )
})

test('Name to id index', () => {
  const db = new ReactiveDb<number, { name: string }>()
  const index = new ReactiveIndex(db, (id, entry) => [[entry.name, id]])
  db.set(1, { name: 'abc' })
  db.set(2, { name: 'xyz' })
  db.set(3, { name: 'abc' })
  expect(index.lookup('abc')).toEqual(new Set([1, 3]))
  expect(index.lookup('xyz')).toEqual(new Set([2]))
  expect(index.lookup('qdr')).toEqual(new Set())
  expect(index.reverseLookup(1)).toEqual(new Set(['abc']))
  expect(index.reverseLookup(2)).toEqual(new Set(['xyz']))
  expect(index.reverseLookup(3)).toEqual(new Set(['abc']))

  db.delete(2)
  expect(index.lookup('xyz')).toEqual(new Set([]))
  db.delete(1)
  expect(index.lookup('abc')).toEqual(new Set([3]))
  db.set(3, { name: 'qdr' })
  expect(index.lookup('abc')).toEqual(new Set())
  expect(index.lookup('qdr')).toEqual(new Set([3]))
})

// Regression test for a bug when the API built on top of ReactiveIndex
// considered an empty set as an existing key value pair.
test('Typical usage of index when looking up if a key exists', () => {
  const db = new ReactiveDb<number, { name: string }>()
  const index = new ReactiveIndex(db, (id, entry) => [[entry.name, id]])
  const allNames = () => [...index.allForward()].map(([name, _]) => name)
  db.set(1, { name: 'abc' })
  db.set(2, { name: 'xyz' })
  db.set(3, { name: 'abc' })
  expect(allNames()).toEqual(['abc', 'xyz'])
  db.delete(2)

  expect(index.hasKey('xyz')).toBe(false)
  expect(allNames()).toEqual(['abc'])
})

test('Parent index', async () => {
  const db = new ReactiveDb<number, { name: string; definedIn?: string }>()
  const qnIndex = new ReactiveIndex(db, (id, entry) => [[entry.name, id]])

  const parent = new ReactiveIndex(db, (id, entry) => {
    if (entry.definedIn) {
      const parents = Array.from(qnIndex.lookup(entry.definedIn))
      return Array.from(parents.map((parent) => [id, parent]))
    }
    return []
  })
  const lookupQn = vi.spyOn(qnIndex, 'lookup')
  const adding = vi.spyOn(parent, 'writeToIndex')
  const removing = vi.spyOn(parent, 'removeFromIndex')
  db.set(1, { name: 'foo', definedIn: 'A' })
  db.set(2, { name: 'A' })
  db.set(3, { name: 'bar', definedIn: 'A' })

  expect(qnIndex.lookup('foo')).toEqual(new Set([1]))
  expect(qnIndex.lookup('A')).toEqual(new Set([2]))
  expect(qnIndex.lookup('bar')).toEqual(new Set([3]))
  expect(parent.lookup(1)).toEqual(new Set([2]))
  expect(parent.lookup(2)).toEqual(new Set())
  expect(parent.lookup(3)).toEqual(new Set([2]))
  expect(parent.reverseLookup(1)).toStrictEqual(new Set())
  expect(parent.reverseLookup(2)).toEqual(new Set([1, 3]))
  expect(parent.reverseLookup(3)).toStrictEqual(new Set())
  expect(adding).toHaveBeenCalledTimes(2)
  expect(removing).toHaveBeenCalledTimes(0)
  expect(lookupQn).toHaveBeenCalledTimes(5)

  db.delete(3)
  await nextTick()
  expect(parent.lookup(3)).toEqual(new Set())
  expect(parent.reverseLookup(2)).toEqual(new Set([1]))
  expect(adding).toHaveBeenCalledTimes(2)
  expect(removing).toHaveBeenCalledTimes(1)
  expect(lookupQn).toHaveBeenCalledTimes(5)
})
