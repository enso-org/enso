/** This file supports the module in `../generated/ast.ts` that is produced by `parser-codegen`. */

export { type Result } from '@/util/result'
import { bail } from '@/util/assert'
import { Err, Error, Ok, type Result } from '@/util/result'

export type Primitive = {
  type: 'primitive'
  value: boolean | number | bigint | string
}
export type DynValue = Primitive | DynSequence | DynResult | DynOption | DynObject
export type DynResult = {
  type: 'result'
  value: Result<DynValue, DynValue>
}
export type DynSequence = {
  type: 'sequence'
  value: Iterable<DynValue>
}
export type DynOption = {
  type: 'option'
  value: DynValue | undefined
}
export type DynObject = {
  type: 'object'
  getFields: () => [string, DynValue][]
}

export const Dyn = {
  Primitive: (value: boolean | number | bigint | string): DynValue => ({
    type: 'primitive',
    value: value,
  }),
  Result: (value: Result<DynValue, DynValue>): DynValue => ({ type: 'result', value: value }),
  Sequence: (value: Iterable<DynValue>): DynValue => ({ type: 'sequence', value: value }),
  Option: (value: DynValue | undefined): DynValue => ({ type: 'option', value: value }),
  Object: (value: LazyObject): DynValue => ({
    type: 'object',
    getFields: value.fields.bind(value),
  }),
} as const

export type ObjectVisitor = (object: LazyObject) => boolean | void
export type ObjectAddressVisitor = (view: DataView, address: number) => boolean | void

/** Base class for objects that lazily deserialize fields when accessed. */
export abstract class LazyObject {
  protected readonly _v: DataView

  protected constructor(view: DataView) {
    this._v = view
  }

  fields(): [string, DynValue][] {
    return []
  }

  visitChildren(_visitor: ObjectVisitor): boolean {
    return false
  }

  children(): LazyObject[] {
    const children: LazyObject[] = []
    this.visitChildren((child) => {
      children.push(child)
    })
    return children
  }
}

type Reader<T> = (view: DataView, address: number) => T

function makeDataView(buffer: ArrayBuffer, address: number) {
  return new DataView(buffer, address)
}

export function readU8(view: DataView, address: number) {
  return view.getUint8(address)
}

export function readU32(view: DataView, address: number) {
  return view.getUint32(address, true)
}

export function readI32(view: DataView, address: number) {
  return view.getInt32(address, true)
}

export function readU64(view: DataView, address: number) {
  return view.getBigUint64(address, true)
}

export function readI64(view: DataView, address: number) {
  return view.getBigInt64(address, true)
}

export function readBool(view: DataView, address: number) {
  return readU8(view, address) !== 0
}

export function readOffset(view: DataView, offset: number) {
  return makeDataView(view.buffer, view.byteOffset + offset)
}

export function readPointer(view: DataView, address: number): DataView {
  return makeDataView(view.buffer, readU32(view, address))
}

const textDecoder = new TextDecoder()

export function readOption<T>(
  view: DataView,
  address: number,
  readElement: Reader<T>,
): T | undefined {
  let result = undefined
  visitOption(view, address, (view, address) => {
    result = readElement(view, address)
  })
  return result
}

export function visitOption(
  view: DataView,
  address: number,
  visitor: ObjectAddressVisitor,
): boolean {
  const discriminant = readU8(view, address)
  switch (discriminant) {
    case 0:
      return false
    case 1:
      return !!visitor(readPointer(view, address + 1), 0)
    default:
      throw new Error(`Invalid Option discriminant: 0x${discriminant.toString(16)}.`)
  }
}

export function readResult<Ok, Err>(
  view: DataView,
  address: number,
  readOk: Reader<Ok>,
  readErr: Reader<Err>,
): Result<Ok, Err> {
  const data = readPointer(view, address)
  const discriminant = readU32(data, 0)
  switch (discriminant) {
    case 0:
      return Ok(readOk(data, 4))
    case 1:
      return Err(readErr(data, 4))
    default:
      throw new Error(`Invalid Result discriminant: 0x${discriminant.toString(16)}.`)
  }
}

export function visitResult(
  view: DataView,
  address: number,
  visitOk: ObjectAddressVisitor | null,
  visitErr: ObjectAddressVisitor | null,
): boolean {
  const data = readPointer(view, address)
  const discriminant = readU32(data, 0)
  switch (discriminant) {
    case 0:
      if (visitOk?.(data, 4)) return true
      return false
    case 1:
      if (visitErr?.(data, 4)) return true
      return false
    default:
      throw new Error(`Invalid Result discriminant: 0x${discriminant.toString(16)}.`)
  }
}

export function visitSequence(
  view: DataView,
  address: number,
  size: number,
  visitor: ObjectAddressVisitor,
): boolean {
  const data = readPointer(view, address)
  let offset = 4
  const end = offset + size * readU32(data, 0)
  while (offset != end) {
    if (visitor(data, offset) === true) return true
    offset += size
  }
  return false
}

export function readSequence<T>(
  view: DataView,
  address: number,
  size: number,
  reader: Reader<T>,
): Iterable<T> {
  const data = readPointer(view, address)
  const offset = 4
  const end = offset + size * readU32(data, 0)
  return new LazySequence(offset, size, end, (offset: number) => reader(data, offset))
}

class LazySequence<T> implements Iterator<T> {
  private offset: number
  private readonly step: number
  private readonly end: number
  private readonly read: (address: number) => T

  constructor(offset: number, step: number, end: number, read: (address: number) => T) {
    this.read = read
    this.offset = offset
    this.step = step
    this.end = end
  }

  [Symbol.iterator]() {
    return this
  }

  public next(): IteratorResult<T> {
    if (this.offset >= this.end) {
      return { done: true, value: undefined }
    }
    const value = this.read(this.offset)
    this.offset += this.step
    return { done: false, value: value }
  }
}

export function readString(view: DataView, address: number): string {
  const data = readPointer(view, address)
  const len = readU32(data, 0)
  const bytes = new Uint8Array(data.buffer, data.byteOffset + 4, len)
  return textDecoder.decode(bytes)
}

export function readEnum<T>(readers: Reader<T>[], view: DataView, address: number): T {
  const data = readPointer(view, address)
  const discriminant = readU32(data, 0)
  const reader = readers[discriminant] ?? bail(`Invalid enum discriminant: ${discriminant}`)
  return reader(data, 4)
}

export function debug(obj: LazyObject): any {
  return debug_(Dyn.Object(obj))
}

function debug_(value: DynValue): any {
  switch (value.type) {
    case 'sequence':
      return Array.from(value.value, debug_)
    case 'result':
      if (value.value.ok) return Ok(debug_(value.value.value))
      else return Err(debug_(value.value.error.payload))
    case 'option':
      if (value.value != null) return debug_(value.value)
      else return undefined
    case 'object': {
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
      return Object.fromEntries(
        value
          .getFields()
          .filter(([name, _]) => !hide.includes(name))
          .map(([name, value]) => [name, debug_(value)]),
      )
    }
    case 'primitive':
      return value.value
  }
}
