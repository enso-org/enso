import { MappedKeyMap, MappedSet } from '@/util/containers'
import { expect, test } from 'vitest'

test('MyMap with number[] keys', () => {
  const map = new MappedKeyMap((key: number[]) => key.join(','))

  const key1 = [1, 2, 3]
  const key2 = [4, 5, 6]
  const key3 = [1, 2, 3]

  expect(map.size).toBe(0)

  expect(map.has(key1)).toBe(false)
  map.set(key1, 'value1')
  expect(map.size).toBe(1)
  expect(map.get(key1)).toBe('value1')
  expect(map.has(key1)).toBe(true)

  map.set(key2, 'value2')
  expect(map.size).toBe(2)
  expect(map.get(key2)).toBe('value2')
  expect(map.has(key2)).toBe(true)

  expect(map.has(key3)).toBe(true)
  expect(map.get(key3)).toBe('value1')
  map.set(key3, 'value3')
  expect(map.size).toBe(2)
  expect(map.get(key1)).toBe('value3')
  expect(map.get(key3)).toBe('value3')

  map.delete(key1)
  expect(map.size).toBe(1)
  expect(map.has(key1)).toBe(false)
  expect(map.get(key1)).toBeUndefined()
  expect(map.get(key2)).toBe('value2')
  expect(map.get(key3)).toBeUndefined()

  map.clear()
  expect(map.size).toBe(0)
  expect(map.has(key2)).toBe(false)
  expect(map.get(key2)).toBe(undefined)
})

test('MappedSet', () => {
  const set = new MappedSet<object>(JSON.stringify)

  const key1 = { a: 1 }
  const key2 = { a: 2 }
  const key3 = { a: 1 }

  expect(set.size).toBe(0)

  expect(set.has(key1)).toBe(false)
  set.add(key1)
  expect(set.size).toBe(1)
  expect(set.has(key1)).toBe(true)

  set.add(key2)
  expect(set.size).toBe(2)
  expect(set.has(key2)).toBe(true)

  expect(set.has(key3)).toBe(true)
  set.add(key3)
  expect(set.size).toBe(2)

  const asArray = Array.from(set)
  expect(asArray).toEqual([key1, key2])

  set.delete(key1)
  expect(set.size).toBe(1)
  expect(set.has(key1)).toBe(false)
  expect(set.has(key2)).toBe(true)

  set.clear()
  expect(set.size).toBe(0)
  expect(set.has(key2)).toBe(false)

  const asArray2 = Array.from(set)
  expect(asArray2).toEqual([])
})
