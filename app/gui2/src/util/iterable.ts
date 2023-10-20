export function* empty(): Generator<never> {}

export function* range(start: number, stop: number, step = start <= stop ? 1 : -1) {
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
