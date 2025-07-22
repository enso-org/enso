/** @file A 2D coordinate. */

/** A 2D coordinate. */
export interface Coordinate2D {
  readonly left: number
  readonly top: number
}

/**
 * A rectangle, including coordinates of every corner.
 */
export interface Rectangle {
  readonly left: number
  readonly top: number
  readonly right: number
  readonly bottom: number
}

/**
 * A bounding box, including all common measurements.
 */
export interface BoundingBox extends Rectangle {
  readonly width: number
  readonly height: number
}

/** A rectangle, including all common measurements. */
export interface DetailedRectangle extends BoundingBox {
  readonly signedWidth: number
  readonly signedHeight: number
}

/**
 * Get a rectangle from two coordinates.
 * @param start - The start coordinate.
 * @param end - The end coordinate.
 * @returns The rectangle.
 */
export function getRectangle(start: Coordinate2D, end: Coordinate2D): Rectangle {
  return {
    left: Math.min(start.left, end.left),
    top: Math.min(start.top, end.top),
    right: Math.max(start.left, end.left),
    bottom: Math.max(start.top, end.top),
  }
}

/**
 * Get a bounding box from two coordinates.
 * @param start - The start coordinate.
 * @param end - The end coordinate.
 * @returns The bounding box.
 */
export function getBoundingBox(start: Coordinate2D, end: Coordinate2D): BoundingBox {
  return {
    ...getRectangle(start, end),
    width: Math.abs(start.left - end.left),
    height: Math.abs(start.top - end.top),
  }
}

/**
 * Get a detailed rectangle from two coordinates.
 * @param start - The start coordinate.
 * @param end - The end coordinate.
 * @returns The rectangle.
 */
export function getDetailedRectangle(start: Coordinate2D, end: Coordinate2D): DetailedRectangle {
  return {
    ...getBoundingBox(start, end),
    signedWidth: end.left - start.left,
    signedHeight: end.top - start.top,
  }
}

/**
 * Get a bounding box from a rectangle.
 * @param rectangle - The rectangle.
 * @returns The bounding box.
 */
export function getBoundingBoxFromRectangle(rectangle: Rectangle): BoundingBox {
  return {
    ...rectangle,
    width: rectangle.right - rectangle.left,
    height: rectangle.bottom - rectangle.top,
  }
}

/**
 * Get a detailed rectangle from a rectangle.
 * @param rectangle - The rectangle.
 * @returns The detailed rectangle.
 */
export function getDetailedRectangleFromRectangle(rectangle: Rectangle): DetailedRectangle {
  return {
    ...getBoundingBoxFromRectangle(rectangle),
    signedWidth: rectangle.right - rectangle.left,
    signedHeight: rectangle.bottom - rectangle.top,
  }
}
