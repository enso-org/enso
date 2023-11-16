import { ReactiveDb, ReactiveIndex } from '@/util/database/reactiveDb'
import { expect, test, vi } from 'vitest'
import { nextTick, reactive } from 'vue'

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
