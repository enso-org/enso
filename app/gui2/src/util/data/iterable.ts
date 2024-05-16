export * from 'shared/util/data/iterable'

export function* filterDefined<T>(iterable: Iterable<T | undefined>): IterableIterator<T> {
  for (const value of iterable) {
    if (value !== undefined) yield value
  }
}

export function every<T>(iter: Iterable<T>, f: (value: T) => boolean): boolean {
  for (const value of iter) if (!f(value)) return false
  return true
}
