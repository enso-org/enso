import { rangeEncloses, rangeIntersects, type SourceRange } from 'shared/yjsModel'
import { expect, test } from 'vitest'

type RangeTest = { a: SourceRange; b: SourceRange }

const equalRanges: RangeTest[] = [
  { a: [0, 0], b: [0, 0] },
  { a: [0, 1], b: [0, 1] },
  { a: [-5, 5], b: [-5, 5] },
]

const totalOverlap: RangeTest[] = [
  { a: [0, 1], b: [0, 0] },
  { a: [0, 2], b: [2, 2] },
  { a: [-1, 1], b: [1, 1] },
  { a: [0, 2], b: [0, 1] },
  { a: [-10, 10], b: [-3, 7] },
  { a: [0, 5], b: [1, 2] },
  { a: [3, 5], b: [3, 4] },
]

const reverseTotalOverlap: RangeTest[] = totalOverlap.map(({ a, b }) => ({ a: b, b: a }))

const noOverlap: RangeTest[] = [
  { a: [0, 1], b: [2, 3] },
  { a: [0, 1], b: [-1, -1] },
  { a: [5, 6], b: [2, 3] },
  { a: [0, 2], b: [-2, -1] },
  { a: [-5, -3], b: [9, 10] },
  { a: [-3, 2], b: [3, 4] },
]

const partialOverlap: RangeTest[] = [
  { a: [0, 3], b: [-1, 1] },
  { a: [0, 1], b: [-1, 0] },
  { a: [0, 0], b: [-1, 0] },
  { a: [0, 2], b: [1, 4] },
  { a: [-8, 0], b: [0, 10] },
]

test.each([...equalRanges, ...totalOverlap])('Range $a should enclose $b', ({ a, b }) =>
  expect(rangeEncloses(a, b)).toBe(true),
)
test.each([...noOverlap, ...partialOverlap, ...reverseTotalOverlap])(
  'Range $a should not enclose $b',
  ({ a, b }) => expect(rangeEncloses(a, b)).toBe(false),
)
test.each([...equalRanges, ...totalOverlap, ...reverseTotalOverlap, ...partialOverlap])(
  'Range $a should intersect $b',
  ({ a, b }) => expect(rangeIntersects(a, b)).toBe(true),
)
test.each([...noOverlap])('Range $a should not intersect $b', ({ a, b }) =>
  expect(rangeIntersects(a, b)).toBe(false),
)
