/**
 * 2D vector, in no particular reference frame. The exact geometric interpretation of the vector
 * depends on the context where it is used.
 */
export class Vec2 {
  readonly x: number
  readonly y: number
  constructor(x: number, y: number) {
    this.x = x
    this.y = y
  }
  static Zero(): Vec2 {
    return new Vec2(0, 0)
  }
  static FromArr(arr: [number, number]): Vec2 {
    return new Vec2(arr[0], arr[1])
  }

  equals(other: Vec2): boolean {
    return this.x === other.x && this.y === other.y
  }
  isZero(): boolean {
    return this.x === 0 && this.y === 0
  }
  scale(scalar: number): Vec2 {
    return new Vec2(this.x * scalar, this.y * scalar)
  }
  distanceSquare(other: Vec2): number {
    const dx = this.x - other.x
    const dy = this.y - other.y
    return dx * dx + dy * dy
  }
  add(other: Vec2): Vec2 {
    return new Vec2(this.x + other.x, this.y + other.y)
  }
  copy(): Vec2 {
    return new Vec2(this.x, this.y)
  }
  addScaled(other: Vec2, scale: number): Vec2 {
    return new Vec2(other.x * scale + this.x, other.y * scale + this.y)
  }
  sub(other: Vec2): Vec2 {
    return new Vec2(this.x - other.x, this.y - other.y)
  }
  lengthSquared(): number {
    return this.x * this.x + this.y * this.y
  }
  length(): number {
    return Math.sqrt(this.lengthSquared())
  }
  min(other: Vec2): Vec2 {
    return new Vec2(Math.min(this.x, other.x), Math.min(this.y, other.y))
  }
  max(other: Vec2): Vec2 {
    return new Vec2(Math.max(this.x, other.x), Math.max(this.y, other.y))
  }
  lerp(to: Vec2, t: number): Vec2 {
    return new Vec2(this.x + (to.x - this.x) * t, this.y + (to.y - this.y) * t)
  }
}
