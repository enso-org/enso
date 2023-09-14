import { isNone, isSome, type Opt } from './opt'

/**
 * Compare two optional numbers. Returns a comparision result like specified for `sort`
 * comparators. None (null or undefined) values may be considered lesser or greater
 * than all others. Null and undefined are not discriminated - they are considered equal.
 *
 * @param a Left operand.
 * @param b Right operand.
 * @param noneValueCmp The result value in case where `a` is none and `b` is a number.
 *   Positive value will make all nones greater than numbres, and negative will make
 *   them lesser. Passing 0 is a Bad Idea™.
 * @returns negative if a < b, positive if a > b, and 0 if a == b.
 */
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
