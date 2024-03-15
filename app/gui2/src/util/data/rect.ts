/** @file Axis-aligned rectangle. Defined in terms of a top-left point and a size. */

import { Vec2 } from '@/util/data/vec2'

/** Axis-aligned rectangle. Defined in terms of a top-left point and a size. */
export class Rect {
  constructor(
    readonly pos: Vec2,
    readonly size: Vec2,
  ) {}

  static Zero: Rect

  static XYWH(x: number, y: number, w: number, h: number): Rect {
    return new Rect(new Vec2(x, y), new Vec2(w, h))
  }

  static FromBounds(left: number, top: number, right: number, bottom: number): Rect {
    return new Rect(new Vec2(left, top), new Vec2(right - left, bottom - top))
  }

  static FromCenterSize(center: Vec2, size: Vec2): Rect {
    return new Rect(center.addScaled(size, -0.5), size)
  }

  static FromDomRect(
    domRect: Readonly<{ x: number; y: number; width: number; height: number }>,
  ): Rect {
    return new Rect(Vec2.FromXY(domRect), Vec2.FromSize(domRect))
  }

  static Bounding(...rects: Rect[]): Rect {
    let left = NaN
    let top = NaN
    let right = NaN
    let bottom = NaN
    for (const rect of rects) {
      if (!(rect.left >= left)) left = rect.left
      if (!(rect.top >= top)) top = rect.top
      if (!(rect.right <= right)) right = rect.right
      if (!(rect.bottom <= bottom)) bottom = rect.bottom
    }
    return this.FromBounds(left, top, right, bottom)
  }

  offsetBy(offset: Vec2): Rect {
    return new Rect(this.pos.add(offset), this.size)
  }

  get left(): number {
    return this.pos.x
  }

  get top(): number {
    return this.pos.y
  }

  get bottom(): number {
    return this.pos.y + this.size.y
  }

  get right(): number {
    return this.pos.x + this.size.x
  }

  get width(): number {
    return this.size.x
  }

  get height(): number {
    return this.size.y
  }

  equals(other: Rect): boolean {
    return this.pos.equals(other.pos) && this.size.equals(other.size)
  }

  within(other: Rect): boolean {
    return (
      this.left >= other.left &&
      this.right <= other.right &&
      this.top >= other.top &&
      this.bottom <= other.bottom
    )
  }

  contains(coord: Partial<Vec2>): boolean {
    return (
      (coord.x == null || (this.left <= coord.x && this.right >= coord.x)) &&
      (coord.y == null || (this.top <= coord.y && this.bottom >= coord.y))
    )
  }

  center(): Vec2 {
    return this.pos.addScaled(this.size, 0.5)
  }

  rangeX(): [number, number] {
    return [this.pos.x, this.pos.x + this.size.x]
  }

  intersectsX(other: Rect): boolean {
    return this.left < other.right && this.right > other.left
  }

  intersectsY(other: Rect): boolean {
    return this.top < other.bottom && this.bottom > other.top
  }

  intersects(other: Rect): boolean {
    return this.intersectsX(other) && this.intersectsY(other)
  }

  /** If this `Rect` already includes `coord`, return `undefined`; otherwise, return a new `Rect` that has been shifted
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
}

Rect.Zero = new Rect(Vec2.Zero, Vec2.Zero)
