/** This file supports the module in `../generated/ast.ts` that is produced by `parser-codegen`. */

import { Err, Ok, type Result } from '@/util/result'

/** Base class for objects that lazily deserialize fields when accessed. */
export abstract class LazyObject {
  protected readonly lazyObjectData: Cursor

  protected constructor(data: Cursor) {
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

  readOption<T>(readElement: (cursor: Cursor) => T): T | undefined {
    const discriminant = this.readU8()
    switch (discriminant) {
      case 0:
        return undefined
      case 1:
        return readElement(this.seek(1).readPointer())
      default:
        throw new Error(`Invalid Option discriminant: 0x${discriminant.toString(16)}.`)
    }
  }

  readResult<Ok, Err>(
    readOk: (cursor: Cursor) => Ok,
    readErr: (cursor: Cursor) => Err,
  ): Result<Ok, Err> {
    const data = this.readPointer()
    const discriminant = data.readU32()
    switch (discriminant) {
      case 0:
        return Ok(readOk(data.seek(4)))
      case 1:
        return Err(readErr(data.seek(4)))
      default:
        throw new Error(`Invalid Result discriminant: 0x${discriminant.toString(16)}.`)
    }
  }

  readPointer(): Cursor {
    const pointee = this.readU32();
    return new Cursor(this.blob.buffer, pointee)
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

  readU64(): bigint {
    return this.blob.getBigUint64(0, true)!
  }

  readI64(): bigint {
    return this.blob.getBigInt64(0, true)!
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

  address(): number {
    return this.blob.byteOffset
  }
}

export function debugHelper(value: any | undefined): any | undefined {
  if (typeof value === 'object') {
    if ('debug' in value && typeof value['debug'] === 'function') {
      return value.debug()
    }
    if (Symbol.iterator in value && typeof value[Symbol.iterator] === 'function') {
      return Array.from(value, debugHelper)
    }
    // FIXME: Include the `hide` reflect property in the schema, and apply it during code generation to avoid magic
    //  strings here.
    const hide = [
      'codeReprBegin',
      'codeReprLen',
      'leftOffsetCodeReprBegin',
      'leftOffsetCodeReprLen',
      'leftOffsetVisible',
      'spanLeftOffsetCodeReprBegin',
      'spanLeftOffsetCodeReprLen',
      'spanLeftOffsetVisible',
    ]
    return Object.fromEntries(Object.entries(value).filter(([key, _val]) => !hide.includes(key)))
  }
  return value
}
