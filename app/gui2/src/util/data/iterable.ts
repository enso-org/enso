export * from 'ydoc-shared/util/data/iterable'

/** TODO: Add docs */
export function* filterDefined<T>(iterable: Iterable<T | undefined>): IterableIterator<T> {
  for (const value of iterable) {
    if (value !== undefined) yield value
  }
}

/** TODO: Add docs */
export function every<T>(iter: Iterable<T>, f: (value: T) => boolean): boolean {
  for (const value of iter) if (!f(value)) return false
  return true
}

/** Return the first element returned by the iterable which meets the condition. */
export function find<T>(iter: Iterable<T>, f: (value: T) => boolean): T | undefined {
  for (const value of iter) {
    if (f(value)) return value
  }
  return undefined
}

/**
 * Return last element returned by the iterable.
 * NOTE: Linear complexity. This function always visits the whole iterable. Using this with an
 * infinite generator will cause an infinite loop.
 */
export function last<T>(iter: Iterable<T>): T | undefined {
  let last
  for (const el of iter) last = el
  return last
}
