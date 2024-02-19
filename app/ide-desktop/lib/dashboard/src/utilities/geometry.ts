/** @file A 2D coordinate. */

/** A 2D coordinate. */
export interface Coordinate2D {
  readonly left: number
  readonly top: number
}

/** A rectangle, including all common measurements. */
export interface DetailedRectangle {
  readonly left: number
  readonly top: number
  readonly right: number
  readonly bottom: number
  readonly width: number
  readonly height: number
  readonly signedWidth: number
  readonly signedHeight: number
}
