import { assert } from '../assert'
import { SourceRange } from './text'

/**
 * A half-open range [from, to). The invariant `from <= to` is enforced. `from` may equal `to`, representing a
 * zero-length range at a specific offset. `from` and `to` are allowed to be negative.
 */
export class Range {
  protected constructor(
    readonly from: number,
    readonly to: number,
  ) {}

  /** @returns A new `Range`, if `from <= to`. */
  static tryFromBounds(from: number, to: number): Range | undefined {
    return from <= to ? new Range(from, to) : undefined
  }

  /** @returns A new `Range`. The caller must ensule that `from <= to`. */
  static unsafeFromBounds(from: number, to: number): Range {
    assert(from <= to)
    return new Range(from, to)
  }

  /** @returns A new from `start` to `start + length`. `length` must be nonnegative. */
  static fromStartAndLength(start: number, length: number): Range {
    assert(length >= 0)
    return new Range(start, start + length)
  }

  /** An empty range at position 0. */
  static empty: Range = Range.emptyAt(0)

  /** @returns An empty range at the given position. */
  static emptyAt(start: number): Range {
    return new Range(start, start)
  }

  /** @returns A nonnegative number representing the length of the range. */
  get length(): number {
    return this.to - this.from
  }

  /** @returns Whether the range length is 0. */
  get empty(): boolean {
    return this.to === this.from
  }

  /**
   * @returns Whether this has the same `from` and `to` as `other`.
   *
   * If `this` and `other` are both of exact type `Range`, this is a complete equality comparison; if either is a
   * derived type, this performs a comparison only with regard to the `Range` data.
   */
  rangeEquals(other: Range | undefined): boolean {
    return !!other && this.from === other.from && this.to === other.to
  }

  /**
   * Create the smallest possible {@link Range} that contains both {@link Range}s.
   * It is not necessary for the two {@link Range}s to overlap.
   */
  merge(other: Range): Range {
    return new Range(Math.min(this.from, other.from), Math.max(this.to, other.to))
  }

  /**
   * Create a new {@link Range} representing *exactly* the sub-ranges that are present in this
   * {@link Range} but not the other.
   *
   * Specifically:
   * - If the other {@link Range} overlaps this one on the left or right, return the single segment
   *   of this {@link Range} that is not overlapped.
   * - If the other {@link Range} is fully within this range, return the two non-overlapped portions
   *   (both left and right) of this {@link Range}.
   * - If the other {@link Range} fully contains this {@link Range}, return an empty array.
   */
  exclude(other: Range): Range[] {
    if (this.from < other.from) {
      const before = new Range(this.from, other.from)
      if (this.to > other.to) return [before, new Range(other.to, this.to)]
      else return [before]
    } else if (this.to > other.to) return [new Range(other.to, this.to)]
    else return []
  }

  /**
   * Expands the range in both directions. If a second argument is provided, the `from` and `to` expansion amounts are
   * given separately.
   */
  expand(by: number, toBy: number = by): Range {
    return new Range(this.from - by, this.to + toBy)
  }

  /**
   * Contracts the range in both directions. If a second argument is provided, the `from` and `to` contraction amounts
   * are given separately. Returns `undefined` if the result would not be a valid range (i.e. it would have negative
   * length if constructed).
   */
  tryContract(by: number, toBy: number = by): Range | undefined {
    return Range.tryFromBounds(this.from + by, this.to - toBy)
  }

  /** @returns The range with both ends displaced by the given amount. */
  shift(offset: number): Range {
    return new Range(this.from + offset, this.to + offset)
  }

  /** @returns A range composed of the closest positions to `from` and `to` that are within `bounds`. */
  clip(bounds: Range): Range {
    const clipPos = (pos: number) => Math.min(Math.max(pos, bounds.from), bounds.to)
    return new Range(clipPos(this.from), clipPos(this.to))
  }

  /** @returns Whether the range fully contains range `inner`--i.e., no part of `inner` extends outside `this`. */
  contains(inner: Range): boolean {
    return this.from <= inner.from && inner.to <= this.to
  }

  /** @returns True if `inner` does not extend outside `outer`, and `inner` is smaller than `outer`. */
  encloses(inner: Range): boolean {
    return (
      (this.from < inner.from && inner.to <= this.to) ||
      (this.from <= inner.from && inner.to < this.to)
    )
  }

  /** @returns Whether `this` ends at or before the start of `other`. */
  endsBefore(other: Range): boolean {
    return this.to <= other.from
  }

  /** @returns Whether the ranges meet. */
  intersects(other: SourceRange): boolean {
    return this.from <= other.to && other.from <= this.to
  }

  /**
   * @returns The part of the given ranges that is contained within both of them (which may be zero-length if they just
   * meet), or `undefined` if they do not meet.
   */
  tryIntersect(other: Range): Range | undefined {
    return Range.tryFromBounds(Math.max(this.from, other.from), Math.min(this.to, other.to))
  }

  /** Extracts the part of a string specified by the range. */
  slice(text: string): string {
    return text.slice(this.from, this.to)
  }
}
