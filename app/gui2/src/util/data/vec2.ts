/**
 * @file 2D vector, in no particular reference frame. The exact geometric interpretation of the
 * vector depends on the context where it is used.
 */

/**
 * 2D vector, in no particular reference frame. The exact geometric interpretation of the vector
 * depends on the context where it is used.
 */
export class Vec2 {
  /** TODO: Add docs */
  constructor(
    readonly x: number,
    readonly y: number,
  ) {}

  static Zero: Vec2
  static Infinity: Vec2

  /** TODO: Add docs */
  static FromTuple(point: [x: number, y: number]): Vec2 {
    return new Vec2(point[0], point[1])
  }

  /** TODO: Add docs */
  static FromXY(point: Readonly<{ x: number; y: number }>): Vec2 {
    return new Vec2(point.x, point.y)
  }

  /** TODO: Add docs */
  static FromSize(point: Readonly<{ width: number; height: number }>): Vec2 {
    return new Vec2(point.width, point.height)
  }

  /** TODO: Add docs */
  static FromClientSize(point: Readonly<{ clientWidth: number; clientHeight: number }>): Vec2 {
    return new Vec2(point.clientWidth, point.clientHeight)
  }

  /** TODO: Add docs */
  static ElementwiseProduct(...values: Vec2[]): Vec2 {
    let x = 1
    let y = 1
    for (const value of values) {
      x *= value.x
      y *= value.y
    }
    return new Vec2(x, y)
  }

  /** TODO: Add docs */
  equals(other: Vec2): boolean {
    return this.x === other.x && this.y === other.y
  }

  /** TODO: Add docs */
  equalsApproximately(other: Vec2, epsilon: number): boolean {
    return Math.abs(this.x - other.x) < epsilon && Math.abs(this.y - other.y) < epsilon
  }

  /** TODO: Add docs */
  isZero(): boolean {
    return this.x === 0 && this.y === 0
  }

  /** TODO: Add docs */
  isFinite(): boolean {
    return Number.isFinite(this.x) && Number.isFinite(this.y)
  }

  /** TODO: Add docs */
  finiteOrZero(): Vec2 {
    return new Vec2(Number.isFinite(this.x) ? this.x : 0, Number.isFinite(this.y) ? this.y : 0)
  }

  /** TODO: Add docs */
  scale(scalar: number): Vec2 {
    return new Vec2(this.x * scalar, this.y * scalar)
  }

  /** TODO: Add docs */
  scaleAround(scalar: number, pivotPoint: Vec2): Vec2 {
    return this.sub(pivotPoint).scale(scalar).add(pivotPoint)
  }

  /** TODO: Add docs */
  distanceSquared(other: Vec2): number {
    const dx = this.x - other.x
    const dy = this.y - other.y
    return dx * dx + dy * dy
  }

  /** TODO: Add docs */
  inverse(): Vec2 {
    return new Vec2(-this.x, -this.y)
  }

  /** TODO: Add docs */
  reciprocal(): Vec2 {
    return new Vec2(1 / this.x, 1 / this.y)
  }

  /** Return a `Vec2` equal to this `Vec2` reflected over the line `y=x`, i.e. with the x and y axes swapped. */
  reflectXY() {
    return new Vec2(this.y, this.x)
  }

  /** TODO: Add docs */
  add(other: Vec2): Vec2 {
    return new Vec2(this.x + other.x, this.y + other.y)
  }

  /** TODO: Add docs */
  copy(): Vec2 {
    return new Vec2(this.x, this.y)
  }

  /** TODO: Add docs */
  addScaled(other: Vec2, scale: number): Vec2 {
    return new Vec2(other.x * scale + this.x, other.y * scale + this.y)
  }

  /** TODO: Add docs */
  sub(other: Vec2): Vec2 {
    return new Vec2(this.x - other.x, this.y - other.y)
  }

  /** TODO: Add docs */
  lengthSquared(): number {
    return this.x * this.x + this.y * this.y
  }

  /** TODO: Add docs */
  length(): number {
    return Math.sqrt(this.lengthSquared())
  }

  /** TODO: Add docs */
  min(other: Vec2): Vec2 {
    return new Vec2(Math.min(this.x, other.x), Math.min(this.y, other.y))
  }

  /** TODO: Add docs */
  max(other: Vec2): Vec2 {
    return new Vec2(Math.max(this.x, other.x), Math.max(this.y, other.y))
  }

  /** TODO: Add docs */
  lerp(to: Vec2, t: number): Vec2 {
    return new Vec2(this.x + (to.x - this.x) * t, this.y + (to.y - this.y) * t)
  }

  /** TODO: Add docs */
  toString(): string {
    return `(${this.x}, ${this.y})`
  }

  /** TODO: Add docs */
  getAxis(axis: 'x' | 'y'): number {
    return axis === 'x' ? this.x : this.y
  }

  /** TODO: Add docs */
  setAxis(axis: 'x' | 'y', value: number) {
    return new Vec2(axis === 'x' ? value : this.x, axis === 'y' ? value : this.y)
  }

  /** TODO: Add docs */
  xy(): { x: number; y: number } {
    return { x: this.x, y: this.y }
  }
}

Vec2.Zero = new Vec2(0, 0)
Vec2.Infinity = new Vec2(Infinity, Infinity)
