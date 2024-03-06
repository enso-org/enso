import { compareOpt } from '@/util/compare'
import { expect, test } from 'vitest'

test.each([
  [1, 2, -1],
  [2, 1, 1],
  [1, 1, 0],
  [null, 1, -1],
  [1, null, 1],
])('Compare %s with %s is %s', (a, b, expected) => {
  expect(compareOpt(a, b)).toBe(expected)
})
