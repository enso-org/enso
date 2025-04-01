/** @file Ranges between two numbers, and sets of multiple ranges with gaps between. */
import { partitionPoint } from '@/util/data/array'
import { Range } from 'ydoc-shared/util/data/range'

/** A {@link Range} with an associated `isMatch` flag. */
export class RangeWithMatch extends Range {
  protected constructor(
    from: number,
    to: number,
    readonly isMatch: boolean,
  ) {
    super(from, to)
  }

  /** @returns A value associating the given `isMatch` value with the specified {@link Range}. */
  static fromRange(range: Range, isMatch: boolean): RangeWithMatch {
    return new RangeWithMatch(range.from, range.to, isMatch)
  }
}

/**
 * Return the included ranges, in addition to the ranges before, between,
 * and after the included ranges.
 */
export function allRanges(ranges: Range[], end: number): Generator<RangeWithMatch>
export function allRanges(ranges: Range[], start: number, end: number): Generator<RangeWithMatch>
export function allRanges(
  ranges: Range[],
  startOrEnd: number,
  end?: number,
): Generator<RangeWithMatch>
/** TODO: Add docs */
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
    yield RangeWithMatch.fromRange(Range.unsafeFromBounds(lastEndIndex, range.from), false)
    yield RangeWithMatch.fromRange(range, true)
    lastEndIndex = range.to
  }
  if (lastEndIndex !== end) {
    yield RangeWithMatch.fromRange(Range.unsafeFromBounds(lastEndIndex, end), false)
  }
}

/** A sorted array of non-intersecting ranges. */
export class MultiRange {
  // This MUST be readonly, otherwise a consumer may mutate it so that it is no longer sorted or
  // non-intersecting.
  readonly ranges: readonly Range[] = []
  /** TODO: Add docs */
  constructor() {}

  private get _ranges(): Range[] {
    return this.ranges as Range[]
  }

  /** TODO: Add docs */
  clear() {
    this._ranges.splice(0, this._ranges.length)
  }

  /** TODO: Add docs */
  insert(range: Range, effectiveRange = range) {
    const start = partitionPoint(this._ranges, (r) => r.to < effectiveRange.from)
    const end = partitionPoint(this._ranges, (r) => r.from <= effectiveRange.to, start)
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

  /** TODO: Add docs */
  remove(range: Range, effectiveRange = range) {
    const start = partitionPoint(this._ranges, (r) => r.to < effectiveRange.from)
    const end = partitionPoint(this._ranges, (r) => r.from <= effectiveRange.to, start)
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
