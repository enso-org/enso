import { intersectionSize } from '@/util/data/set'
import { expect, test } from 'vitest'

test.each`
  left         | right        | expected
  ${[]}        | ${[]}        | ${0}
  ${[3]}       | ${[]}        | ${0}
  ${[]}        | ${[3]}       | ${0}
  ${[3]}       | ${[3]}       | ${1}
  ${[1, 2, 3]} | ${[2, 3, 4]} | ${2}
`('Count common in sets $left and $right', ({ left, right, expected }) => {
  expect(intersectionSize(new Set(left), new Set(right))).toBe(expected)
})
