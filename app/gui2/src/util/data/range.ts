/** @file Ranges between two numbers, and sets of multiple ranges with gaps between. */

import { partitionPoint } from '@/util/data/array'

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

  /** Create a new {@link Range} representing *exactly* the sub-ranges that are present in this
   * {@link Range} but not the other.
   *
   * Specifically:
   * - If the other {@link Range} overlaps this one on the left or right, return the single segment
   *   of this {@link Range} that is not overlapped.
   * - If the other {@link Range} is fully within this range, return the two non-overlapped portions
   *   (both left and right) of this {@link Range}.
   * - If the other {@link Range} fully contains this {@link Range}, return an empty array. */
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
  // This MUST be readonly, otherwise a consumer may mutate it so that it is no longer sorted or
  // non-intersecting.
  readonly ranges: readonly Range[] = []
  constructor() {}

  private get _ranges(): Range[] {
    return this.ranges as Range[]
  }

  clear() {
    this._ranges.splice(0, this._ranges.length)
  }

  insert(range: Range, effectiveRange = range) {
    const start = partitionPoint(this._ranges, (r) => r.end < effectiveRange.start)
    const end = partitionPoint(this._ranges, (r) => r.start <= effectiveRange.end, start)
    let finalRange = range
    if (end !== start) {
      const startRange = this._ranges[start]
      if (startRange) finalRange = finalRange.merge(startRange)
    }
    if (end - 1 > start) {
      const endRange = this._ranges[end - 1]
      if (endRange) finalRange = finalRange.merge(endRange)
    }
    return this._ranges.splice(start, end - start, finalRange)[0]!
  }

  remove(range: Range, effectiveRange = range) {
    const start = partitionPoint(this._ranges, (r) => r.end < effectiveRange.start)
    const end = partitionPoint(this._ranges, (r) => r.start <= effectiveRange.end, start)
    const finalRanges: Range[] = []
    if (end !== start) {
      const startRange = this._ranges[start]
      if (startRange) finalRanges.push(...startRange.exclude(range))
    }
    if (end - 1 > start) {
      const endRange = this._ranges[end - 1]
      if (endRange) finalRanges.push(...endRange.exclude(range))
    }
    return this._ranges.splice(start, end - start, ...finalRanges)
  }
}
