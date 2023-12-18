import { MultiRange, Range } from '@/util/data/range'
import { expect, test } from 'vitest'

function r(...r: [start: number, end: number][]) {
  return r.map(({ 0: start, 1: end }) => new Range(start, end))
}

function mr(...r: [start: number, end: number][]) {
  const m = new MultiRange()
  for (const range of r) {
    m.insert(new Range(range[0], range[1]))
  }
  return m
}

function add(m: MultiRange, ...r: [start: number, end: number][]) {
  for (const range of r) {
    m.insert(new Range(range[0], range[1]))
  }
  return m
}

function sub(m: MultiRange, ...r: [start: number, end: number][]) {
  for (const range of r) {
    m.remove(new Range(range[0], range[1]))
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
