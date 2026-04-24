import { fc, test as fctest } from '@fast-check/vitest'
import { expect, test } from 'vitest'
import { computed, reactive, ref, shallowRef, toRaw, unref } from 'vue'
import { cloneDeepUnref } from '../reactive'
import { setsIntersect } from '../set'

class MappedSet<T, U> {
  private readonly set: Set<U> = new Set()

  constructor(private readonly f: (value: T) => U) {}

  has(value: T): boolean {
    return this.set.has(this.f(value))
  }

  add(value: T): Set<U> {
    return this.set.add(this.f(value))
  }

  keys(): SetIterator<U> {
    return this.set.keys()
  }
}

const primitiveValues = [
  undefined,
  null,
  0,
  3,
  '',
  'hello',
  true,
  false,
  Symbol('foo'),
  BigInt(123),
]

/** Tests that the given sets don't contain any of the same keys. */
function expectDisjoint(a: Pick<Set<unknown>, 'keys'>, b: Pick<Set<unknown>, 'has'>) {
  expect(setsIntersect(a, b)).toBe(false)
}

function checkUnrefEqualCollectingObjects(input: unknown, copy: unknown) {
  const inputObjects = new MappedSet<object, object>(toRaw)
  const copyObjects = new MappedSet<object, object>(toRaw)
  const recurse = (rawInput: unknown, copy: unknown) => {
    const input = unref(rawInput)
    if (typeof input === 'object' && input !== null) {
      if (typeof copy !== 'object') return null
      expect(copy).toBeTypeOf('object')
      inputObjects.add(input)
      copyObjects.add(copy as object)
      expect(Object.keys(input as any)).toEqual(Object.keys(copy as any))
      const inputValues = Object.values(input as any)
      const copyValues = Object.values(copy as any)
      inputValues.forEach((value, i) => recurse(value, copyValues[i]))
    } else {
      if (input !== copy) return null
    }
  }
  recurse(input, copy)
  return { inputObjects, copyObjects }
}

/**
 * Recursively checks that after applying {@link unref} to `input`, the result is equal to `copy`,
 * but no object in `copy` aliases any object in (the unreffed) `input`.
 *
 * Equality: Objects are compared only in regard to values of enumerable string keys (this includes
 * array elements).
 *
 * Proxies: Vue proxies are supported. Aliasing of either the proxy object or the underlying will be
 * detected. Any proxies not unwrapped by Vue's {@link toRaw} are not supported and will not have
 * full aliasing detection.
 */
function expectUnrefDeepCopy(input: unknown, copy: unknown) {
  const equalResult = checkUnrefEqualCollectingObjects(input, copy)
  expect(equalResult).not.toBeNull()
  const { inputObjects, copyObjects } = equalResult!
  expectDisjoint(inputObjects, copyObjects)
}

const nonPrimitiveIdempotentCases = [
  ...primitiveValues.map((v) => [v]),
  ...primitiveValues.map((v) => [1, v]),
  ...primitiveValues.map((v) => [1, [v]]),
  ...primitiveValues.map((v) => ({ key: v })),
  ...primitiveValues.map((v) => [{ key: v }]),
  ...primitiveValues.map((v) => reactive({ key: v })),
  ...primitiveValues.map((v) => [reactive({ key: v })]),
]
const idempotentCases = [...primitiveValues, ...nonPrimitiveIdempotentCases]
test.each(idempotentCases)('cloneDeepUnref: Clones values not containing refs', (input) =>
  expectUnrefDeepCopy(input, cloneDeepUnref(input as unknown)),
)

const sharedObject = { key: 'value' }
test.each([
  ...nonPrimitiveIdempotentCases.map((input) => ({ input, copyWithAliasing: input })),
  {
    input: [sharedObject],
    copyWithAliasing: [sharedObject],
  },
  {
    input: [reactive(sharedObject)],
    copyWithAliasing: [reactive(sharedObject)],
  },
  {
    input: [reactive(sharedObject)],
    copyWithAliasing: [sharedObject],
  },
  {
    input: [{ x: 1, y: reactive(sharedObject) }],
    copyWithAliasing: [{ x: 1, y: sharedObject }],
  },
])('expectUnrefDeepCopy internals: Aliasing is detected', ({ input, copyWithAliasing }) => {
  const equalResult = checkUnrefEqualCollectingObjects(input, copyWithAliasing)
  expect(equalResult).not.toBeNull()
  const { inputObjects, copyObjects } = equalResult!
  expect(setsIntersect(inputObjects, copyObjects)).toBe(true)
})

function getter<T>(value: T): () => T {
  return () => value
}

const structureContainingReactivityArbitrary = () =>
  fc.letrec((tie) => ({
    node: fc.oneof(
      { depthSize: 'medium' },
      tie('primitive'),
      tie('object'),
      tie('array'),
      tie('refPrimitive'),
      tie('reactiveObject'),
      tie('reactiveArray'),
      tie('shallowRef'),
    ),
    object: fc.record({
      left: tie('node'),
      right: tie('node'),
    }),
    array: fc.array(tie('node')),
    primitive: fc.constantFrom(...primitiveValues),
    refPrimitive: tie('primitive').map(ref),
    reactiveObject: (tie('object') as fc.Arbitrary<object>).map(reactive),
    reactiveArray: (tie('array') as fc.Arbitrary<unknown[]>).map(reactive),
    shallowRef: tie('node').map(shallowRef),
    getter: tie('node').map(getter),
    computed: (tie('getter') as fc.Arbitrary<() => unknown>).map((getter) => computed(getter)),
  }))

fctest.prop(structureContainingReactivityArbitrary())(
  'expectUnrefDeepCopy: Unrefs and clones values containing refs',
  ({ node }: { node: unknown }) => expectUnrefDeepCopy(node, cloneDeepUnref(node)),
)
