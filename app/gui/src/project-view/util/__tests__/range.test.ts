import { MultiRange } from '@/util/data/range'
import { expect, test } from 'vitest'
import { Range } from 'ydoc-shared/util/data/range'

function r(...r: [from: number, to: number][]) {
  return r.map(({ 0: from, 1: to }) => Range.tryFromBounds(from, to)!)
}

function mr(...r: [from: number, to: number][]) {
  const m = new MultiRange()
  for (const range of r) {
    m.insert(Range.tryFromBounds(range[0], range[1])!)
  }
  return m
}

function add(m: MultiRange, ...r: [from: number, to: number][]) {
  for (const range of r) {
    m.insert(Range.tryFromBounds(range[0], range[1])!)
  }
  return m
}

function sub(m: MultiRange, ...r: [from: number, to: number][]) {
  for (const range of r) {
    m.remove(Range.tryFromBounds(range[0], range[1])!)
  }
  return m
}

test('MultiRange', () => {
  expect(mr([0, 10], [10, 20]).ranges).toEqual(r([0, 20]))
  expect(mr([0, 8], [5, 15], [12, 20]).ranges).toEqual(r([0, 20]))
  expect(mr([0, 8], [12, 20], [5, 15]).ranges).toEqual(r([0, 20]))
  expect(mr([0, 8], [5, 15], [12, 20]).ranges).toEqual(r([0, 20]))
  expect(mr([0, 8], [12, 20]).ranges).toEqual(r([0, 8], [12, 20]))
  expect(mr([12, 20], [0, 8]).ranges).toEqual(r([0, 8], [12, 20]))
  expect(sub(mr([12, 20], [0, 8]), [5, 15]).ranges).toEqual(r([0, 5], [15, 20]))
  expect(add(sub(mr([12, 20], [0, 8]), [5, 15]), [12, 20]).ranges).toEqual(r([0, 5], [12, 20]))
  expect(add(sub(mr([12, 20], [0, 8]), [5, 15]), [12, 15]).ranges).toEqual(r([0, 5], [12, 20]))
  expect(add(sub(mr([12, 20], [0, 8]), [5, 15]), [12, 14]).ranges).toEqual(
    r([0, 5], [12, 14], [15, 20]),
  )
  expect(sub(mr([0, 20]), [-Infinity, 0]).ranges).toEqual(r([0, 20]))
  expect(sub(mr([0, 20]), [-Infinity, 5]).ranges).toEqual(r([5, 20]))
})
