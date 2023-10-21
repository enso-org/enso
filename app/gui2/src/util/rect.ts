import { Vec2 } from '@/util/vec2'

/**
 * Axis-aligned rectangle. Defined in terms of a top-left point and a size.
 */
export class Rect {
  pos: Vec2
  size: Vec2
  constructor(pos: Vec2, size: Vec2) {
    this.pos = pos
    this.size = size
  }

  equals(other: Rect): boolean {
    return this.pos.equals(other.pos) && this.size.equals(other.size)
  }

  static Zero(): Rect {
    return new Rect(Vec2.Zero(), Vec2.Zero())
  }

  center(): Vec2 {
    return this.pos.addScaled(this.size, 0.5)
  }

  rangeX(): [number, number] {
    return [this.pos.x, this.pos.x + this.size.x]
  }
}
