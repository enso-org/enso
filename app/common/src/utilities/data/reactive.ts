import { isRef, unref, type ComputedRef, type Ref } from 'vue'

/**
 * A value that may be wrapped in a ref. Note that this is different from the Vue type with the same
 * name.
 */
type MaybeRef<T> = Ref<T> | ComputedRef<T> | T

/**
 * A type that may recursively contain {@link Ref}s or getter functions, and can be recursively
 * unwrapped by {@link cloneDeepUnref}
 */
export type MaybeRefDeep<T> = MaybeRef<
  // eslint-disable-next-line @typescript-eslint/no-unsafe-function-type
  T extends Function ? T
  : T extends object ?
    {
      [Property in keyof T]: MaybeRefDeep<T[Property]>
    }
  : T
>

/**
 * Helper function for cloning deep objects where the level and key is provided to the callback
 * function.
 */
function _cloneDeep<T>(
  value: MaybeRefDeep<T>,
  customize?: (val: MaybeRefDeep<T>, key: string, level: number) => T | undefined,
  currentKey: string = '',
  currentLevel: number = 0,
): T {
  if (customize) {
    const result = customize(value, currentKey, currentLevel)
    if (result === undefined && isRef(value)) {
      return result as T
    }
    if (result !== undefined) {
      return result
    }
  }

  if (Array.isArray(value)) {
    return value.map((val, index) =>
      _cloneDeep(val, customize, String(index), currentLevel + 1),
    ) as unknown as T
  }

  if (typeof value === 'object' && isPlainObject(value)) {
    const entries = Object.entries(value).map(([key, val]) => [
      key,
      _cloneDeep(val, customize, key, currentLevel + 1),
    ])
    return Object.fromEntries(entries)
  }

  return value as T
}

function cloneDeep<T>(
  value: MaybeRefDeep<T>,
  customize?: (val: MaybeRefDeep<T>, key: string, level: number) => T | undefined,
): T {
  return _cloneDeep(value, customize)
}

/** Recursively clone the provided value, unwrapping any references found. */
export function cloneDeepUnref<T>(obj: MaybeRefDeep<T>, unrefGetters = false): T {
  return cloneDeep(obj, (val, key, level) => {
    // Check if we're at the top level and the key is 'queryKey'
    //
    // If so, take the recursive descent where we resolve getters to values as well as refs.
    if (level === 1 && key === 'queryKey') {
      return cloneDeepUnref(val, true)
    }

    // Resolve getters to values if specified.
    if (unrefGetters && isFunction(val)) {
      return cloneDeepUnref(val(), unrefGetters)
    }

    // Unref refs and continue to recurse into the value.
    if (isRef(val)) {
      return cloneDeepUnref(unref(val), unrefGetters)
    }

    return undefined
  })
}

// eslint-disable-next-line @typescript-eslint/no-wrapper-object-types
function isPlainObject(value: unknown): value is Object {
  if (Object.prototype.toString.call(value) !== '[object Object]') {
    return false
  }

  const prototype = Object.getPrototypeOf(value)
  return prototype === null || prototype === Object.prototype
}

// eslint-disable-next-line @typescript-eslint/no-unsafe-function-type
function isFunction(value: unknown): value is Function {
  return typeof value === 'function'
}
