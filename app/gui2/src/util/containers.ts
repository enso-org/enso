import { assertDefined, assertEqual } from '@/util/assert'
import { expect, test } from 'vitest'

/**
 * Map that supports Object-based keys.
 *
 * Internally keys are converted to strings using the provided {@link keyMapper} function and then compared.
 *
 * @template Key The type of the keys.
 * @template Value The type of the values.
 */
export class ObjectKeyedMap<Key extends Object, Value> {
  /** The inner map that stores the values. */
  private readonly map = new Map<string, [Key, Value]>()

  /** Construct a new map, optionally setting a custom key mapper.
   *
   * @param keyMapper The function that maps the keys to strings. By default, strings are generated using {@link JSON.stringify}.
   *
   */
  constructor(private readonly keyMapper: (key: Key) => string = JSON.stringify) {}

  /** Set the value for the given key. */
  set(key: Key, value: Value): this {
    const innerKey = this.keyMapper(key)
    this.map.set(innerKey, [key, value])
    return this
  }

  /** Get the value for the given key, or `undefined` if it does not exist. */
  get(key: Key): Value | undefined {
    const innerKey = this.keyMapper(key)
    const entry = this.map.get(innerKey)
    return entry ? entry[1] : undefined
  }

  /** Check if the map contains a value for the given key. */
  has(key: Key): boolean {
    const innerKey = this.keyMapper(key)
    return this.map.has(innerKey)
  }

  /** Remove the value for the given key. */
  delete(key: Key): boolean {
    const innerKey = this.keyMapper(key)
    return this.map.delete(innerKey)
  }

  /** Remove all values from the map. */
  clear(): void {
    this.map.clear()
  }

  /** Get the number of values in the map. */
  get size(): number {
    return this.map.size
  }

  /** Iterate over the values in the map. */
  *[Symbol.iterator](): IterableIterator<[Key, Value]> {
    for (const [_innerKey, [key, value]] of this.map.entries()) {
      yield [key, value]
    }
  }
}

/**
 * Set that supports Object-based keys.
 *
 * Internally keys are converted to strings using the provided {@link keyMapper} function and then compared.
 *
 * @template Key The type of the keys.
 */
export class ObjectKeyedSet<Key extends Object> {
  /** The inner set that stores the keys. */
  private readonly set = new Set<string>()

  /** Construct a new set, optionally setting a custom key mapper.
   *
   * @param keyMapper The function that maps the keys to strings. By default, strings are generated using {@link JSON.stringify}.
   *
   */
  constructor(private readonly keyMapper: (key: Key) => string = JSON.stringify) {}

  /** Add the given key to the set. */
  add(key: Key): this {
    const innerKey = this.keyMapper(key)
    this.set.add(innerKey)
    return this
  }

  /** Check if the set contains the given key. */
  has(key: Key): boolean {
    const innerKey = this.keyMapper(key)
    return this.set.has(innerKey)
  }

  /** Remove the given key from the set. */
  delete(key: Key): boolean {
    const innerKey = this.keyMapper(key)
    return this.set.delete(innerKey)
  }

  /** Remove all keys from the set. */
  clear(): void {
    this.set.clear()
  }

  /** Get the number of keys in the set. */
  get size(): number {
    return this.set.size
  }

  /** Iterate over the keys in the set. */
  *[Symbol.iterator](): IterableIterator<Key> {
    for (const innerKey of this.set) {
      yield JSON.parse(innerKey) as Key
    }
  }
}

/** Stack that always has at least one element.
 *
 * It is meant to be used with scope-based operations, thus it does not provide direct `push` and `pop` methods.
 */
export class NonEmptyStack<T> {
  /** The "actual" stack of elements. */
  private readonly stack: T[]

  /** Construct a new stack with the given initial value.
   *
   * The value will serve as an always-present bottom of the stack.
   */
  constructor(initial: T) {
    this.stack = [initial]
  }

  /** Temporary pushes the given value to the stack and calls the callback. */
  withPushed<R>(value: T, callback: (value: T) => R): { value: T; result?: R } {
    this.stack.push(value)
    let result = undefined
    try {
      result = callback(value)
    } finally {
      const popped = this.stack.pop()
      assertDefined(popped, 'Stack is empty.')
      assertEqual(popped, value, 'Stack is inconsistent: expected to pop the pushed value.')
    }
    return { value, result }
  }

  /** Get the top-most element of the stack. */
  get top(): T {
    const ret = this.stack[this.stack.length - 1]
    assertDefined(ret, 'Stack is empty.')
    return ret
  }

  /** Iterate over the stack values from the top to the bottom. */
  *valuesFromTop(): Iterable<T> {
    for (let i = this.stack.length - 1; i >= 0; i--) {
      const value = this.stack[i]
      // Be defensive against the stack mutation during the iteration.
      if (value != null) {
        yield value
      }
    }
  }
}

if (import.meta.vitest) {
  test('MyMap with number[] keys', () => {
    const map = new ObjectKeyedMap<number[], string>()

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

  test('ObjectKeyedSet', () => {
    const set = new ObjectKeyedSet<{ a: number }>()

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
  })
}
