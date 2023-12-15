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

export function* map<T, U>(iter: Iterable<T>, map: (value: T) => U) {
  for (const value of iter) {
    yield map(value)
  }
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
