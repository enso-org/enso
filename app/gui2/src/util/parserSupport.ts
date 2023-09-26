export class LazyObject {
  protected readonly lazyObjectData: Cursor

  constructor(data: Cursor) {
    this.lazyObjectData = data
  }

  debug(): {} {
    return {}
  }
}

export const builtin = {
  Array: Array,
} as const

export class Cursor {
  private readonly blob: DataView

  constructor(buffer: ArrayBuffer, address: number) {
    this.blob = new DataView(buffer, address)
  }

  *readSequence<T>(readElement: (cursor: Cursor) => T, elementSize: number): Iterable<T> {
    const data = this.readPointer()
    let count = data.readU32()
    let offset = 4
    while (count > 0) {
      yield readElement(data.seek(offset))
      count--
      offset += elementSize
    }
  }

  readOption<T>(readElement: (cursor: Cursor) => T): T | null {
    const discriminant = this.readU8()
    switch (discriminant) {
      case 0:
        return null
      case 1:
        return readElement(this.seek(1).readPointer())
      default:
        throw new Error(`Invalid Option discriminant: 0x${discriminant.toString(16)}.`)
    }
  }

  readResult<Ok, Err>(readOk: (cursor: Cursor) => Ok, readErr: (cursor: Cursor) => Err): Ok | Err {
    const data = this.readPointer()
    const discriminant = data.readU32()
    switch (discriminant) {
      case 0:
        return readOk(data.seek(4))
      case 1:
        return readErr(data.seek(4))
      default:
        throw new Error(`Invalid Result discriminant: 0x${discriminant.toString(16)}.`)
    }
  }

  readPointer(): Cursor {
    return new Cursor(this.blob.buffer, this.readU32())
  }

  readU8(): number {
    return this.blob.getUint8(0)!
  }

  readU32(): number {
    return this.blob.getUint32(0, true)!
  }

  readI32(): number {
    return this.blob.getInt32(0, true)!
  }

  readU64(): number {
    const lo = this.readU32()
    const hi = this.seek(4).readU32()
    //if (hi !== 0) {
    //  throw new RangeError()
    //}
    return lo
  }

  readBool(): boolean {
    const value = this.readU8()
    switch (value) {
      case 0:
        return false
      case 1:
        return true
      default:
        throw new Error(
          `Invalid boolean: 0x${value.toString(16)} @ 0x${this.blob.byteOffset.toString(16)}.`,
        )
    }
  }

  readString(): string {
    const data = this.readPointer()
    const len = data.readU32()
    const bytes = data.blob.buffer.slice(data.blob.byteOffset + 4, data.blob.byteOffset + 4 + len)
    return new TextDecoder().decode(bytes)
  }

  seek(offset: number): Cursor {
    return new Cursor(this.blob.buffer, this.blob.byteOffset + offset)
  }
}

export function debugHelper(value: any): object | null {
  if (value === null) {
    return null
  }
  if (typeof value['debug'] === 'function') {
    return value.debug()
  }
  if (typeof value[Symbol.iterator] === 'function') {
    return Array.from(value, debugHelper)
  }
  return value
}
