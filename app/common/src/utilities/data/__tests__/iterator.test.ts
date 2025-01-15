import { expect, test } from 'vitest'
import * as iter from '../iter'

interface IteratorCase<T> {
  iterable: Iterable<T>
  soleValue: T | undefined
  first: T | undefined
  last: T | undefined
  count: number
}

function makeCases(): IteratorCase<unknown>[] {
  return [
    {
      iterable: iter.empty(),
      soleValue: undefined,
      first: undefined,
      last: undefined,
      count: 0,
    },
    {
      iterable: iter.chain(iter.empty(), iter.empty()),
      soleValue: undefined,
      first: undefined,
      last: undefined,
      count: 0,
    },
    {
      iterable: iter.chain(iter.empty(), ['a'], iter.empty()),
      soleValue: 'a',
      first: 'a',
      last: 'a',
      count: 1,
    },
    {
      iterable: iter.range(10, 11),
      soleValue: 10,
      first: 10,
      last: 10,
      count: 1,
    },
    {
      iterable: iter.range(10, 20),
      soleValue: undefined,
      first: 10,
      last: 19,
      count: 10,
    },
    {
      iterable: iter.range(20, 10),
      soleValue: undefined,
      first: 20,
      last: 11,
      count: 10,
    },
    {
      iterable: [],
      soleValue: undefined,
      first: undefined,
      last: undefined,
      count: 0,
    },
    {
      iterable: ['a'],
      soleValue: 'a',
      first: 'a',
      last: 'a',
      count: 1,
    },
    {
      iterable: ['a', 'b'],
      soleValue: undefined,
      first: 'a',
      last: 'b',
      count: 2,
    },
    {
      iterable: iter.filterDefined([undefined, 'a', undefined, 'b', undefined]),
      soleValue: undefined,
      first: 'a',
      last: 'b',
      count: 2,
    },
    {
      iterable: iter.filter([7, 'a', 8, 'b', 9], (el) => typeof el === 'string'),
      soleValue: undefined,
      first: 'a',
      last: 'b',
      count: 2,
    },
    {
      iterable: iter.zip(['a', 'b'], iter.range(1, 2)),
      soleValue: ['a', 1],
      first: ['a', 1],
      last: ['a', 1],
      count: 1,
    },
    {
      iterable: iter.zip(['a', 'b'], iter.range(1, 3)),
      soleValue: undefined,
      first: ['a', 1],
      last: ['b', 2],
      count: 2,
    },
    {
      iterable: iter.zip(['a', 'b'], iter.range(1, 4)),
      soleValue: undefined,
      first: ['a', 1],
      last: ['b', 2],
      count: 2,
    },
    {
      iterable: iter.zipLongest(['a', 'b'], iter.range(1, 2)),
      soleValue: undefined,
      first: ['a', 1],
      last: ['b', undefined],
      count: 2,
    },
    {
      iterable: iter.zipLongest(['a', 'b'], iter.range(1, 3)),
      soleValue: undefined,
      first: ['a', 1],
      last: ['b', 2],
      count: 2,
    },
    {
      iterable: iter.zipLongest(['a', 'b'], iter.range(1, 4)),
      soleValue: undefined,
      first: ['a', 1],
      last: [undefined, 3],
      count: 3,
    },
  ]
}

test.each(makeCases())('tryGetSoleValue: case %#', ({ iterable, soleValue }) => {
  expect(iter.tryGetSoleValue(iterable)).toEqual(soleValue)
})

test.each(makeCases())('last: case %#', ({ iterable, last }) => {
  expect(iter.last(iterable)).toEqual(last)
})

test.each(makeCases())('count: case %#', ({ iterable, count }) => {
  expect(iter.count(iterable)).toEqual(count)
})
