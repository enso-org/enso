/** @file Axis-aligned rectangle. Defined in terms of a top-left point and a size. */

import { Vec2 } from '@/util/data/vec2'
import { markRaw } from 'vue'

/** Axis-aligned rectangle. Defined in terms of a top-left point and a size. */
export class Rect {
  /** TODO: Add docs */
  constructor(
    readonly pos: Vec2,
    readonly size: Vec2,
  ) {}

  static Zero: Rect

  /** TODO: Add docs */
  static XYWH(x: number, y: number, w: number, h: number): Rect {
    return new Rect(new Vec2(x, y), new Vec2(w, h))
  }

  /** TODO: Add docs */
  static FromBounds(left: number, top: number, right: number, bottom: number): Rect {
    return new Rect(new Vec2(left, top), new Vec2(right - left, bottom - top))
  }

  /** TODO: Add docs */
  static FromCenterSize(center: Vec2, size: Vec2): Rect {
    return new Rect(center.addScaled(size, -0.5), size)
  }

  /** TODO: Add docs */
  static FromDomRect(
    domRect: Readonly<{ x: number; y: number; width: number; height: number }>,
  ): Rect {
    return new Rect(Vec2.FromXY(domRect), Vec2.FromSize(domRect))
  }

  /** Create a minimum rectangle that contains all passed-in rectangles. */
  static Bounding(): undefined
  /** Create a minimum rectangle that contains all passed-in rectangles. */
  static Bounding(rect: Rect, ...rects: Rect[]): Rect
  /** Create a minimum rectangle that contains all passed-in rectangles. */
  static Bounding(...rects: Rect[]): Rect | undefined
  /** Create a minimum rectangle that contains all passed-in rectangles. */
  static Bounding(...rects: Rect[]): Rect | undefined {
    if (rects.length === 0) return
    let left = Infinity
    let top = Infinity
    let right = -Infinity
    let bottom = -Infinity
    for (const rect of rects) {
      left = Math.min(left, rect.left)
      top = Math.min(top, rect.top)
      right = Math.max(right, rect.right)
      bottom = Math.max(bottom, rect.bottom)
    }
    return this.FromBounds(left, top, right, bottom)
  }

  /** Create a minimum rectangle that contains all passed-in points. */
  static FromPoints(): undefined
  /** Create a minimum rectangle that contains all passed-in points. */
  static FromPoints(point: Vec2, ...points: Vec2[]): Rect
  /** Create a minimum rectangle that contains all passed-in points. */
  static FromPoints(...points: Vec2[]): Rect | undefined
  /** Create a minimum rectangle that contains all passed-in points. */
  static FromPoints(...points: Vec2[]): Rect | undefined {
    if (points.length === 0) return
    let minX = Infinity
    let minY = Infinity
    let maxX = -Infinity
    let maxY = -Infinity
    for (const point of points) {
      minX = Math.min(minX, point.x)
      minY = Math.min(minY, point.y)
      maxX = Math.max(maxX, point.x)
      maxY = Math.max(maxY, point.y)
    }
    return this.FromBounds(minX, minY, maxX, maxY)
  }

  /** TODO: Add docs */
  static Equal(a: Rect, b: Rect): boolean
  /** TODO: Add docs */
  static Equal(a: Rect | null, b: Rect | null): boolean
  /** TODO: Add docs */
  static Equal(a: Rect | undefined, b: Rect | undefined): boolean
  /** TODO: Add docs */
  static Equal(a: Rect | null | undefined, b: Rect | null | undefined): boolean {
    if (!a && !b) return true
    if (!a || !b) return false
    return a.equals(b)
  }

  /** TODO: Add docs */
  withBounds({ top, left, bottom, right }: Partial<Bounds>): Rect {
    return Rect.FromBounds(
      left ?? this.left,
      top ?? this.top,
      right ?? this.right,
      bottom ?? this.bottom,
    )
  }

  /** TODO: Add docs */
  withBoundsClamped(bounds: Partial<Bounds>): Rect {
    const left = bounds?.left != null ? Math.min(bounds.left, this.right) : this.left
    const right = bounds?.right != null ? Math.max(bounds.right, left) : this.right
    const top = bounds?.top != null ? Math.min(bounds.top, this.bottom) : this.top
    const bottom = bounds?.bottom != null ? Math.max(bounds.bottom, top) : this.bottom
    return Rect.FromBounds(left, top, right, bottom)
  }

  /** TODO: Add docs */
  isFinite(): boolean {
    return this.pos.isFinite() && this.size.isFinite()
  }

  /** TODO: Add docs */
  offsetBy(offset: Vec2): Rect {
    return new Rect(this.pos.add(offset), this.size)
  }

  /** TODO: Add docs */
  get left(): number {
    return this.pos.x
  }

  /** TODO: Add docs */
  get top(): number {
    return this.pos.y
  }

  /** TODO: Add docs */
  get bottom(): number {
    return this.pos.y + this.size.y
  }

  /** TODO: Add docs */
  get right(): number {
    return this.pos.x + this.size.x
  }

  /** TODO: Add docs */
  get width(): number {
    return this.size.x
  }

  /** TODO: Add docs */
  get height(): number {
    return this.size.y
  }

  /** TODO: Add docs */
  equals(other: Rect): boolean {
    return this.pos.equals(other.pos) && this.size.equals(other.size)
  }

  /** TODO: Add docs */
  equalsApproximately(other: Rect, epsilon: number): boolean {
    return (
      this.pos.equalsApproximately(other.pos, epsilon) &&
      this.size.equalsApproximately(other.size, epsilon)
    )
  }

  /** TODO: Add docs */
  within(other: Rect): boolean {
    return (
      this.left >= other.left &&
      this.right <= other.right &&
      this.top >= other.top &&
      this.bottom <= other.bottom
    )
  }

  /** TODO: Add docs */
  contains(coord: Partial<Vec2>): boolean {
    return (
      (coord.x == null || (this.left <= coord.x && this.right >= coord.x)) &&
      (coord.y == null || (this.top <= coord.y && this.bottom >= coord.y))
    )
  }

  /** TODO: Add docs */
  center(): Vec2 {
    return this.pos.addScaled(this.size, 0.5)
  }

  /** TODO: Add docs */
  rangeX(): [number, number] {
    return [this.pos.x, this.pos.x + this.size.x]
  }

  /** TODO: Add docs */
  intersectsX(other: Rect): boolean {
    return this.left < other.right && this.right > other.left
  }

  /** TODO: Add docs */
  intersectsY(other: Rect): boolean {
    return this.top < other.bottom && this.bottom > other.top
  }

  /** TODO: Add docs */
  intersects(other: Rect): boolean {
    return this.intersectsX(other) && this.intersectsY(other)
  }

  /**
   * If this `Rect` already includes `coord`, return `undefined`; otherwise, return a new `Rect` that has been shifted
   *  by the minimum distance that causes it to include the coordinate. The coordinate may be a point or may specify
   *  only an `x` or `y` bound to leave the other dimension unchanged.
   */
  offsetToInclude(coord: Partial<Vec2>): Rect | undefined {
    const newX =
      coord.x == null ? undefined
      : coord.x < this.left ? coord.x
      : coord.x > this.right ? coord.x - this.width
      : undefined
    const newY =
      coord.y == null ? undefined
      : coord.y < this.top ? coord.y
      : coord.y > this.bottom ? coord.y - this.height
      : undefined
    if (newX == null && newY == null) return
    return new Rect(new Vec2(newX ?? this.pos.x, newY ?? this.pos.y), this.size)
  }

  /**
   * Returns a value that will compare equal for any two rects `a` and `b` if `a.equals(b)`, and
   *  `a.isFinite() && b.isFinite()`. The result of comparing keys from two `Rect`s that don't satisfy `isFinite` are
   *  unspecified, but a key returned from a non-finite `Rect` will never compare equal to a key return from any finite
   *  Rect.
   *  ---------------------------------------------------
   *  | KEYS EQUAL      | a is finite | a is not finite |
   *  ---------------------------------------------------
   *  | b is finite     | a.equals(b) | false           |
   *  | b is not finite | false       | unspecified     |
   *  ---------------------------------------------------
   */
  key(): string {
    return [this.top, this.bottom, this.left, this.right].join(':')
  }

  /**
   * Return a `Rect` equal to this `Rect` reflected over the line `y=x`, i.e. with the x and y axes of all coordinates
   *  swapped.
   */
  reflectXY() {
    return new Rect(this.pos.reflectXY(), this.size.reflectXY())
  }

  /** TODO: Add docs */
  toDomRect(): DOMRect {
    return DOMRect.fromRect({
      x: this.pos.x,
      y: this.pos.y,
      width: this.size.x,
      height: this.size.y,
    })
  }

  /** TODO: Add docs */
  expand(padding: number): Rect {
    const padVector = new Vec2(padding, padding)
    return new Rect(this.pos.sub(padVector), this.size.add(padVector).add(padVector))
  }

  /** Get an SVG path representation of this rectangle, optionally with corner rounding. */
  asSvgPath(r: number = 0): string {
    const {
      pos: { x, y },
      size: { x: w, y: h },
    } = this
    if (r == 0) {
      return `M${x} ${y} h${w} v${h} h${-w} z`
    } else {
      const ix = x + r
      const iw = w - r * 2
      const ih = h - r * 2
      const arc = `a${r} ${r} 0 0 1`
      return `M${ix},${y} h${iw} ${arc} ${r},${r} v${ih}  ${arc} ${-r},${r} h${-iw}  ${arc} ${-r},${-r} v${-ih}  ${arc} ${r},${-r} z`
    }
  }
}

// All Rect instances are immutable, therefore we don't need to track them with reactivity.
markRaw(Rect.prototype)

Rect.Zero = new Rect(Vec2.Zero, Vec2.Zero)

export interface Bounds {
  left: number
  right: number
  top: number
  bottom: number
}
export interface BoundsSet {
  left?: boolean
  right?: boolean
  top?: boolean
  bottom?: boolean
}
