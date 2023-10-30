import { Vec2 } from '@/util/vec2'

/**
 * Axis-aligned rectangle. Defined in terms of a top-left point and a size.
 */
export class Rect {
  constructor(
    readonly pos: Vec2,
    readonly size: Vec2,
  ) {}

  static Zero = new Rect(Vec2.Zero, Vec2.Zero)

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
