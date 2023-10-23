import { binarySearch } from '@/util/array'

export interface RangeWithMatch {
  readonly start: number
  readonly end: number
  readonly isMatch: boolean
}

/** Return the included ranges, in addition to the ranges before, between,
 * and after the included ranges. */
export function allRanges(ranges: Range[], end: number): Generator<RangeWithMatch>
export function allRanges(ranges: Range[], start: number, end: number): Generator<RangeWithMatch>
export function allRanges(
  ranges: Range[],
  startOrEnd: number,
  end?: number,
): Generator<RangeWithMatch>
export function* allRanges(
  ranges: Range[],
  start: number,
  end?: number,
): Generator<RangeWithMatch> {
  if (end == null) {
    end = start
    start = 0
  }
  let lastEndIndex = start
  for (const range of ranges) {
    yield { start: lastEndIndex, end: range.start, isMatch: false }
    yield { ...range, isMatch: true }
    lastEndIndex = range.end
  }
  if (lastEndIndex !== end) {
    yield { start: lastEndIndex, end, isMatch: false }
  }
}

export class Range {
  constructor(
    readonly start: number,
    readonly end: number,
  ) {}

  /** Create the smallest possible {@link Range} that contains both {@link Range}s.
   * It is not necessary for the two {@link Range}s to overlap. */
  merge(other: Range): Range {
    return new Range(Math.min(this.start, other.start), Math.max(this.end, other.end))
  }

  exclude(other: Range): Range[] {
    if (this.start < other.start) {
      const before = new Range(this.start, other.start)
      if (this.end > other.end) return [before, new Range(other.end, this.end)]
      else return [before]
    } else if (this.end > other.end) return [new Range(other.end, this.end)]
    else return []
  }

  intersects(other: Range) {
    return this.start < other.end && this.end > other.start
  }

  expand(by: number) {
    return new Range(this.start - by, this.end + by)
  }
}

/** A sorted array of non-intersecting ranges. */
export class MultiRange {
  // It is fine for this to be JS-`#private` as it is re-exposed as a public member anyway.
  readonly #ranges: Range[] = []
  // This MUST be private, otherwise a consumer may mutate it so that it is no longer sorted or
  // non-intersecting.
  readonly ranges: readonly Range[] = this.#ranges
  constructor() {}

  clear() {
    this.#ranges.splice(0, this.#ranges.length)
  }

  insert(range: Range, effectiveRange = range) {
    const start = binarySearch(this.#ranges, (r) => r.end >= effectiveRange.start)
    const end = binarySearch(this.#ranges, (r) => r.start > effectiveRange.end, start)
    let finalRange = range
    if (end !== start) {
      const startRange = this.#ranges[start]
      if (startRange) finalRange = finalRange.merge(startRange)
    }
    if (end - 1 > start) {
      const endRange = this.#ranges[end - 1]
      if (endRange) finalRange = finalRange.merge(endRange)
    }
    return this.#ranges.splice(start, end - start, finalRange)[0]!
  }

  remove(range: Range, effectiveRange = range) {
    const start = binarySearch(this.#ranges, (r) => r.end >= effectiveRange.start)
    const end = binarySearch(this.#ranges, (r) => r.start > effectiveRange.end, start)
    const finalRanges: Range[] = []
    if (end !== start) {
      const startRange = this.#ranges[start]
      if (startRange) finalRanges.push(...startRange.exclude(range))
    }
    if (end - 1 > start) {
      const endRange = this.#ranges[end - 1]
      if (endRange) finalRanges.push(...endRange.exclude(range))
    }
    return this.#ranges.splice(start, end - start, ...finalRanges)
  }
}

if (import.meta.vitest) {
  const { test, expect } = import.meta.vitest

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
}
