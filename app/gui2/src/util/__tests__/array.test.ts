import { partitionPoint } from '@/util/array'
import { fc, test as fcTest } from '@fast-check/vitest'
import { expect } from 'vitest'

const isEven = (n: number) => n % 2 === 0
const isOdd = (n: number) => n % 2 === 1

fcTest.prop({
  evens: fc.array(fc.nat(1_000_000_000)).map((a) => a.map((n) => n * 2)),
  odds: fc.array(fc.nat(1_000_000_000)).map((a) => a.map((n) => n * 2 + 1)),
})('partitionPoint (even/odd)', ({ evens, odds }) => {
  expect(partitionPoint([...evens, ...odds], isEven)).toEqual(evens.length)
  expect(partitionPoint([...odds, ...evens], isOdd)).toEqual(odds.length)
})

fcTest.prop({
  arr: fc
    .array(fc.float())
    .map((a) => ({ arr: a.sort((a, b) => a - b), i: Math.floor(Math.random() * a.length) })),
})('partitionPoint (ascending)', ({ arr: { arr, i } }) => {
  const target = arr[i]!
  expect(partitionPoint(arr, (n) => n < target)).toEqual(i)
})

fcTest.prop({
  arr: fc
    .array(fc.float())
    .map((a) => ({ arr: a.sort((a, b) => b - a), i: Math.floor(Math.random() * a.length) })),
})('partitionPoint (descending)', ({ arr: { arr, i } }) => {
  const target = arr[i]!
  expect(partitionPoint(arr, (n) => n > target)).toEqual(i)
})
