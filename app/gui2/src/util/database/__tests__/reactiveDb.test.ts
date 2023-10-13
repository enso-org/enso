import { ReactiveDb, ReactiveIndex } from '@/util/database/reactiveDb'
import { expect, test, vi } from 'vitest'
import { nextTick, reactive, watch } from 'vue'

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
  const db = new ReactiveDb()
  const index = new ReactiveIndex(db, (id, entry, index) => index(entry.name, id))
  const adding = vi.spyOn(index, 'writeToIndex')
  const removing = vi.spyOn(index, 'removeFromIndex')
  db.set(1, { name: 'abc' })
  db.set(2, { name: 'xyz' })
  db.set(3, { name: 'abc' })
  db.delete(2)
  expect(adding).toHaveBeenCalledTimes(3)
  expect(removing).toHaveBeenCalledTimes(1)
  db.set(1, { name: 'qdr' })
  expect(adding).toHaveBeenCalledTimes(4)
  expect(removing).toHaveBeenCalledTimes(2)
})

test('Name to id index', () => {
  const db = new ReactiveDb()
  const index = new ReactiveIndex(db, (id, entry, index) => index(entry.name, id))
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
  const db = new ReactiveDb()
  const qnIndex = reactive(
    new ReactiveIndex(db, (id, entry, index) => {
      console.log('index qnIndex')
      index(entry.name, id)
    }),
  )
  const children = new ReactiveIndex(db, (id, entry, index, onDelete) => {
    console.log('index children')
    const stop = watch(
      qnIndex,
      (qnIndex) => {
        qnIndex.lookup(entry.definedIn).forEach((parentId) => index(parentId, id))
      },
      { immediate: true },
    )
    onDelete(() => stop())
  })
  //const lookupQn = vi.spyOn(qnIndex, 'lookup')
  //const adding = vi.spyOn(children, 'writeToIndex')
  //const removing = vi.spyOn(children, 'removeFromIndex')
  db.set(1, { name: 'foo', definedIn: 'A' })
  db.set(2, { name: 'A' })
  db.set(3, { name: 'bar', definedIn: 'A' })
  await nextTick()
  expect(qnIndex.lookup('foo')).toEqual(new Set([1]))
  expect(qnIndex.lookup('A')).toEqual(new Set([2]))
  expect(qnIndex.lookup('bar')).toEqual(new Set([3]))
  // expect(children.lookup(1)).toEqual(new Set())
  // expect(children.lookup(2)).toEqual(new Set([1, 3]))
  // expect(children.lookup(3)).toEqual(new Set())
  // expect(children.reverseLookup(1)).toStrictEqual(new Set([2]))
  // expect(children.reverseLookup(2)).toEqual(new Set())
  // expect(children.reverseLookup(3)).toStrictEqual(new Set([2]))
  //expect(adding).toHaveBeenCalledTimes(2)
  //expect(removing).toHaveBeenCalledTimes(0)
  //expect(lookupQn).toHaveBeenCalledTimes(5)

  db.delete(3)
  await nextTick()
  expect(qnIndex.lookup('foo')).toEqual(new Set([1]))
  expect(qnIndex.lookup('A')).toEqual(new Set([2]))
  expect(qnIndex.lookup('bar')).toEqual(new Set([]))
  // expect(children.lookup(2)).toEqual(new Set([1]))
  // expect(children.reverseLookup(3)).toEqual(new Set())
  // expect(adding).toHaveBeenCalledTimes(2)
  // expect(removing).toHaveBeenCalledTimes(1)
  // expect(lookupQn).toHaveBeenCalledTimes(5)
})
