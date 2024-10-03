export * from 'ydoc-shared/util/data/iterable'

export function* filterDefined<T>(iterable: Iterable<T | undefined>): IterableIterator<T> {
  for (const value of iterable) {
    if (value !== undefined) yield value
  }
}

export function every<T>(iter: Iterable<T>, f: (value: T) => boolean): boolean {
  for (const value of iter) if (!f(value)) return false
  return true
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
