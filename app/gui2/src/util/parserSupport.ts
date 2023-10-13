/** This file supports the module in `../generated/ast.ts` that is produced by `parser-codegen`. */

export { type Result } from '@/util/result'
import { Err, Error, Ok, type Result } from '@/util/result'
import { bail } from './assert'

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
  protected readonly _v: DataView

  protected constructor(view: DataView) {
    this._v = view
  }

  fields(): [string, DynValue][] {
    return []
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
  const discriminant = readU8(view, address)
  switch (discriminant) {
    case 0:
      return undefined
    case 1:
      return readElement(readPointer(view, address + 1), 0)
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
export function* readSequence<T>(view: DataView, address: number, size: number, reader: Reader<T>) {
  const data = readPointer(view, address)
  let count = readU32(data, 0)
  let offset = 4
  while (count > 0) {
    yield reader(data, offset)
    count--
    offset += size
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
