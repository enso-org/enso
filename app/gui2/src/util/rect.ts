import { Vec2 } from '@/util/vec2'

/**
 * Axis-aligned rectangle. Defined in terms of a top-left point and a size.
 */
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

  static FromDomRect(domRect: DOMRect): Rect {
    return new Rect(new Vec2(domRect.x, domRect.y), new Vec2(domRect.width, domRect.height))
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
}

Rect.Zero = new Rect(Vec2.Zero, Vec2.Zero)
