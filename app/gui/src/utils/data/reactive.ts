import * as objects from 'enso-common/src/utilities/data/object'
import { unref, type MaybeRef } from 'vue'

/**
 * A type that may recursively contain refs or getter functions, and can be recursively
 * unwrapped by {@link cloneDeepUnref}. Note that this is not exactly the same as the tanstack type
 * of the same name: That type allows getters, even though tanstack's `cloneDeepUnref` only
 * evaluates them under certain conditions.
 */
export type MaybeRefDeep<T> = MaybeRef<
  // Disallow functions because they are must likely intended to be getters, which are not evaluated
  // by {@link cloneDeepUnref}.
  T extends () => unknown ? never
  : T extends object ?
    {
      [Property in keyof T]: MaybeRefDeep<T[Property]>
    }
  : T
>

function throwIfDev(message: string) {
  DEV: throw new Error(message)
}

/** Recursively clone the provided value, unwrapping any references found. */
export function cloneDeepUnref<T>(obj: MaybeRefDeep<T>): T {
  const v = unref(obj)
  if (typeof v === 'function') {
    // This is not allowed because the value is likely to be intended to be a getter, and
    // tanstack-query's internal version of the function evaluates getters only in certain
    // circumstances. A `computed` value may be used instead.
    const message = 'BUG: Function found in data passed to `cloneDeepUnref`.'
    console.error(message)
    throwIfDev(message)
    // Error recovery
    return cloneDeepUnref(v())
  }
  return (
    Array.isArray(v) ? v.map(cloneDeepUnref)
    : typeof v === 'object' && isPlainObject(v) ?
      objects.mapEntries(v, (_key, v) => cloneDeepUnref(v))
    : v) as T
}

// eslint-disable-next-line @typescript-eslint/no-wrapper-object-types
function isPlainObject(value: unknown): value is Object {
  if (Object.prototype.toString.call(value) !== '[object Object]') {
    return false
  }

  const prototype = Object.getPrototypeOf(value)
  return prototype === null || prototype === Object.prototype
}
