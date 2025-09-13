/** @file Tests for `LruCache.ts`. */
import { fc, test } from '@fast-check/vitest'
import { describe, expect, vi } from 'vitest'

import { LRUCache } from '../LruCache'

describe('LruCache', () => {
  test('retreive cached results', () => {
    const cache = new LRUCache(5)
    const anObject = {}
    cache.set('a', 1)
    cache.set('b', 2)
    cache.set('c', '3')
    cache.set('d', null)
    cache.set('e', anObject)
    expect(cache.get('b')).eq(2)
    expect(cache.get('a')).eq(1)
    expect(cache.get('c')).eq('3')
    expect(cache.get('d')).eq(null)
    expect(cache.get('e')).eq(anObject)
  })

  test('remove entries when over capacity (base case)', () => {
    const cache = new LRUCache(2)
    cache.set('a', 1)
    cache.set('b', 2)
    cache.set('c', 3)
    cache.set('d', 4)
    cache.set('e', 5)
    expect(cache.peek('a')).eq(undefined)
    // b and c being resolvable here is possible but not guaranteed
    expect(cache.peek('b')).oneOf([2, undefined])
    expect(cache.peek('c')).oneOf([3, undefined])
    // latest N must always resolve
    expect(cache.peek('d')).eq(4)
    expect(cache.peek('e')).eq(5)
  })

  // Thorough automated version of above test.
  test.prop({
    capacity: fc.integer({ min: 1, max: 64 }),
    insertsOverCapacity: fc.nat(200),
  })('remove entries when over capacity (automated case)', ({ capacity, insertsOverCapacity }) => {
    const actualEvicts: number[] = []
    const cache = new LRUCache(capacity, (_, k: number) => actualEvicts.push(k))
    const totalInserts = capacity + insertsOverCapacity
    for (let i = 0; i < totalInserts; i++) cache.set(i, `val: ${i}`)

    // Cache guarantees storing from cap to cap*2 most recent entries, so we have 3 test cases:
    // - entries older than cap*2 should NOT be present
    // - latest cap entries should ALWAYS be present
    // - entries inbetween may not be present, but they should contain correct value if present
    let i = 0
    for (; i < totalInserts - capacity * 2; i++) expect(cache.peek(i)).eq(undefined)
    for (; i < totalInserts - capacity; i++) expect(cache.peek(i)).oneOf([undefined, `val: ${i}`])
    for (; i < totalInserts; i++) expect(cache.peek(i)).eq(`val: ${i}`)

    const sequential = (k: number) => [...Array(k).keys()]
    // Check if evicted entries match above guarantees as well
    expect(actualEvicts.length).toBeLessThanOrEqual(totalInserts - capacity)
    expect(actualEvicts.length).toBeGreaterThan(totalInserts - capacity * 2)
    expect(actualEvicts).toStrictEqual(sequential(actualEvicts.length))

    // Finally, everything else should be evicted once cache is cleaned up.
    cache.clear()
    expect(actualEvicts).toStrictEqual(sequential(totalInserts))
  })

  test('allow taking out entries', () => {
    const onEvict = vi.fn()
    const cache = new LRUCache(5, onEvict)
    cache.set('a', 1)
    cache.set('b', 2)
    expect(cache.take('a')).eq(1)
    expect(onEvict).not.toHaveBeenCalled()
  })

  test('preserve recently accessed entries', () => {
    const cache = new LRUCache(3)
    cache.set('a', 1)
    cache.set('b', 2)
    cache.set('c', 3)
    expect(cache.get('a')).eq(1)
    cache.set('d', 4)
    cache.set('e', 5)
    expect(cache.get('b')).eq(undefined)
    expect(cache.get('c')).eq(undefined)
    expect(cache.get('d')).eq(4)
    expect(cache.get('e')).eq(5)
    expect(cache.get('a')).eq(1)
  })

  test('call onEvict handlers when over capacity', () => {
    const onEvict = vi.fn()
    const cache = new LRUCache(2, onEvict)
    cache.set('a', 1)
    cache.set('b', 2)
    cache.set('c', 3)
    cache.set('d', 4)
    cache.set('e', 5)
    cache.set('f', 6)
    expect(onEvict).nthCalledWith(1, 1, 'a')
    expect(onEvict).nthCalledWith(2, 2, 'b')
    expect(onEvict).nthCalledWith(3, 3, 'c')
    expect(onEvict).nthCalledWith(4, 4, 'd')
    expect(onEvict).toHaveBeenCalledTimes(4)
  })
})
