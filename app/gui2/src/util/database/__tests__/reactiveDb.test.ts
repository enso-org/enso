import { expect, test } from 'vitest'
import { ReactiveDb } from '@/util/database/reactiveDb'
import { ReactiveIndex } from '@/util/database/reactiveDb'
import { setIfUndefined } from 'lib0/map'
import { watch, reactive, nextTick } from 'vue'

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

test('Name to id index', () => {
  const db = new ReactiveDb()
  const index = new ReactiveIndex(db,
								  (key, value, index) => {
									const set = setIfUndefined(index, value.name, () => new Set())
									set.add(key)
  },
								  (key, value, index) => {
									index.set(key, value.name)
  })
  db.set(1, { name: 'abc' })
  db.set(2, { name: 'xyz' })
  db.set(3, { name: 'abc' })
  expect(index.lookup('abc')).toEqual(new Set([1, 3]))
  expect(index.lookup('xyz')).toEqual(new Set([2]))
  expect(index.reverseLookup(1)).toEqual('abc')
  expect(index.reverseLookup(2)).toEqual('xyz')
})

test('Parent index', async () => {
  const db = new ReactiveDb()
  const qnIndex = reactive(new ReactiveIndex(db,
								  (key, value, index) => {
									const set = setIfUndefined(index, value.name, () => new Set())
									set.add(key)
  },
								  (key, value, index) => {
									index.set(key, value.name)
  }))
  const children = new ReactiveIndex(db, (key, value, index) => {
	watch(qnIndex, (qnIndex) => {
	  if (!qnIndex.lookup(value.definedIn)) return
	  for (const id of qnIndex.lookup(value.definedIn).values()) {
		const set = setIfUndefined(index, id, () => new Set())
		set.add(key)
	  }
	}, { immediate: true })
  },
									 (key, value, index) => {
									
  })
  db.set(1, { name: 'foo', definedIn: 'A' })
  db.set(2, { name: 'A' })
  db.set(3, { name: 'bar', definedIn: 'A' })
  await nextTick()
  expect(children.lookup(2)).toEqual(new Set([1, 3]))
  //expect(children.reverseLookup(1)).toStrictEqual(2)
})
