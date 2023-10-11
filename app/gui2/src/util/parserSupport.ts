/** This file supports the module in `../generated/ast.ts` that is produced by `parser-codegen`. */

export { type Result } from '@/util/result'
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

/** Base class for objects that lazily deserialize fields when accessed. */
export abstract class LazyObject {
  protected readonly lazyObjectData: Cursor

  protected constructor(data: Cursor) {
    this.lazyObjectData = data
  }

  fields(): [string, DynValue][] {
    return []
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
    const pointee = this.readU32()
    return new Cursor(this.blob.buffer, pointee)
  }

  readU8(): number {
    return this.blob.getUint8(0)
  }

  readU32(): number {
    return this.blob.getUint32(0, true)
  }

  readI32(): number {
    return this.blob.getInt32(0, true)
  }

  readU64(): bigint {
    return this.blob.getBigUint64(0, true)
  }

  readI64(): bigint {
    return this.blob.getBigInt64(0, true)
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

export function validateSpans(obj: LazyObject, initialPos?: number): number {
  const state = { pos: initialPos ?? 0 }
  validateSpans_(Dyn.Object(obj), state)
  return state.pos
}

function validateSpans_(value: DynValue, state: { pos: number }) {
  switch (value.type) {
    case 'sequence':
      for (const elem of value.value) validateSpans_(elem, state)
      break
    case 'result':
      if (value.value.ok) validateSpans_(value.value.value, state)
      else validateSpans_(value.value.error.payload, state)
      break
    case 'option':
      if (value.value != null) validateSpans_(value.value, state)
      break
    case 'object':
      return validateObjectSpans(value, state)
    case 'primitive':
      break
  }
}

function validateObjectSpans(value: DynObject, state: { pos: number }) {
  const fields = new Map(value.getFields())
  const whitespaceStart =
    fields.get('whitespaceStartInCodeParsed') ?? fields.get('whitespaceStartInCodeBuffer')
  const whitespaceLength =
    fields.get('whitespaceLengthInCodeParsed') ?? fields.get('whitespaceLengthInCodeBuffer')
  const codeStart = fields.get('startInCodeBuffer')
  const codeLength = fields.get('lengthInCodeBuffer')
  const childrenCodeLength = fields.get('childrenLengthInCodeParsed')
  if (
    !(
      whitespaceLength?.type === 'primitive' &&
      whitespaceLength.value === 0 &&
      codeLength?.type === 'primitive' &&
      codeLength?.value === 0
    )
  ) {
    if (whitespaceStart?.type === 'primitive' && whitespaceStart.value !== state.pos)
      throw new Error(`Span error (whitespace) in: ${JSON.stringify(debug_(value))}.`)
    if (whitespaceLength?.type === 'primitive') state.pos += whitespaceLength.value as number
    if (codeStart?.type === 'primitive' && codeStart.value !== state.pos)
      throw new Error('Span error (code).')
    if (codeLength?.type === 'primitive') state.pos += codeLength.value as number
  }
  let endPos: number | undefined
  if (childrenCodeLength?.type === 'primitive')
    endPos = state.pos + (childrenCodeLength.value as number)
  for (const entry of fields) {
    const [_name, value] = entry
    validateSpans_(value, state)
  }
  if (endPos != null && state.pos !== endPos) throw new Error('Span error (children).')
}
