/** @file Functions for manipulating {@link Iterable}s. */

export function* empty(): Generator<never> {}

export function* range(start: number, stop: number, step = start <= stop ? 1 : -1) {
  if ((step > 0 && start > stop) || (step < 0 && start < stop)) {
    throw new Error(
      "The range's step is in the wrong direction - please use Infinity or -Infinity as the endpoint for an infinite range.",
    )
  }
  if (start <= stop) {
    while (start < stop) {
      yield start
      start += step
    }
  } else {
    while (start > stop) {
      yield start
      start += step
    }
  }
}

export function* map<T, U>(iter: Iterable<T>, map: (value: T) => U): IterableIterator<U> {
  for (const value of iter) {
    yield map(value)
  }
}

export function* filter<T>(iter: Iterable<T>, include: (value: T) => boolean): IterableIterator<T> {
  for (const value of iter) if (include(value)) yield value
}

export function* chain<T>(...iters: Iterable<T>[]) {
  for (const iter of iters) {
    yield* iter
  }
}

export function* zip<T, U>(left: Iterable<T>, right: Iterable<U>): Generator<[T, U]> {
  const leftIterator = left[Symbol.iterator]()
  const rightIterator = right[Symbol.iterator]()
  while (true) {
    const leftResult = leftIterator.next()
    const rightResult = rightIterator.next()
    if (leftResult.done || rightResult.done) break
    yield [leftResult.value, rightResult.value]
  }
}

export function* zipLongest<T, U>(
  left: Iterable<T>,
  right: Iterable<U>,
): Generator<[T | undefined, U | undefined]> {
  const leftIterator = left[Symbol.iterator]()
  const rightIterator = right[Symbol.iterator]()
  while (true) {
    const leftResult = leftIterator.next()
    const rightResult = rightIterator.next()
    if (leftResult.done && rightResult.done) break
    yield [
      leftResult.done ? undefined : leftResult.value,
      rightResult.done ? undefined : rightResult.value,
    ]
  }
}

export function tryGetSoleValue<T>(iter: Iterable<T>): T | undefined {
  const iterator = iter[Symbol.iterator]()
  const result = iterator.next()
  if (result.done) return
  const excessResult = iterator.next()
  if (!excessResult.done) return
  return result.value
}

/** Utility to simplify consuming an iterator a part at a time. */
export class Resumable<T> {
  private readonly iterator: Iterator<T>
  private current: IteratorResult<T>
  constructor(iterable: Iterable<T>) {
    this.iterator = iterable[Symbol.iterator]()
    this.current = this.iterator.next()
  }

  peek() {
    return this.current.done ? undefined : this.current.value
  }

  advance() {
    this.current = this.iterator.next()
  }

  /** The given function peeks at the current value. If the function returns `true`, the current value will be advanced
   *  and the function called again; if it returns `false`, the peeked value remains current and `advanceWhile` returns.
   */
  advanceWhile(f: (value: T) => boolean) {
    while (!this.current.done && f(this.current.value)) {
      this.current = this.iterator.next()
    }
  }

  /** Apply the given function to all values remaining in the iterator. */
  forEach(f: (value: T) => void) {
    while (!this.current.done) {
      f(this.current.value)
      this.current = this.iterator.next()
    }
  }
}
