import { isNone, isSome, type Opt } from './opt'

export function compareOpt(a: Opt<number>, b: Opt<number>, noneValueCmp: number = -1): number {
  if (isSome(a) && isSome(b)) {
    return a - b
  } else if (isNone(a) && isNone(b)) {
    return 0
  } else {
    return isNone(a) ? noneValueCmp : -noneValueCmp
  }
}

if (import.meta.vitest) {
  const { test, expect } = import.meta.vitest
  test.each([
    [1, 2, -1],
    [2, 1, 1],
    [1, 1, 0],
    [null, 1, -1],
    [1, null, 1],
  ])('Compare %s with %s is %s', (a, b, expected) => {
    expect(compareOpt(a, b)).toBe(expected)
  })
}
