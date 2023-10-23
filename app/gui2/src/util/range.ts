export interface Range {
  start: number
  end: number
}

export interface RangeWithMatch {
  start: number
  end: number
  isMatch: boolean
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
