/** @file A 2D coordinate. */

/** A 2D coordinate. */
export interface Coordinate2D {
  readonly left: number
  readonly top: number
}

/** A rectangle. */
export interface Rectangle {
  readonly left: number
  readonly top: number
  readonly width: number
  readonly height: number
}
