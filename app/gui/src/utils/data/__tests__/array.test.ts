import { expect, test } from 'vitest'
import * as array from '../array'

interface TransposeCase {
  matrix: number[][]
  expected: number[][]
}

const transposeCases: TransposeCase[] = [
  { matrix: [], expected: [] },
  { matrix: [[]], expected: [[]] },
  { matrix: [[1]], expected: [[1]] },
  { matrix: [[1, 2]], expected: [[1], [2]] },
  { matrix: [[1], [2]], expected: [[1, 2]] },
  {
    matrix: [
      [1, 2, 3],
      [4, 5, 6],
    ],
    expected: [
      [1, 4],
      [2, 5],
      [3, 6],
    ],
  },
]

test.each(transposeCases)('transpose: case %#', ({ matrix, expected }) => {
  const transposed = array.transpose(matrix)
  expect(transposed).toStrictEqual(expected)
})
