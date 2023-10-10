/** Generates code lazily deserializing from an application-specific binary format. */

import ts from 'typescript'
import { casesOrThrow, modifiers } from './util.js'

const { factory: tsf } = ts

// === Definitions ===

const noneType = tsf.createTypeReferenceNode('undefined')
const cursorFieldIdent = tsf.createIdentifier('lazyObjectData')
const POINTER_SIZE: number = 4
// Symbols exported by the `parserSupport` module.
export const supportImports = {
  LazyObject: false,
  Cursor: false,
  Result: true,
  DynValue: true,
  Dyn: false,
} as const
export const support = {
  LazyObject: tsf.createIdentifier('LazyObject'),
  Cursor: tsf.createTypeReferenceNode(tsf.createIdentifier('Cursor')),
  Result: (t0: ts.TypeNode, t1: ts.TypeNode) =>
    tsf.createTypeReferenceNode(tsf.createIdentifier('Result'), [t0, t1]),
  DynValue: 'DynValue',
  Dyn: tsf.createIdentifier('Dyn'),
} as const

const cursorMethods = {
  readString: primitiveReader('readString'),
  readBool: primitiveReader('readBool'),
  readU32: primitiveReader('readU32'),
  readI32: primitiveReader('readI32'),
  readU64: primitiveReader('readU64'),
  readI64: primitiveReader('readI64'),
  readPointer: primitiveReader('readPointer'),
  readSequence: readerTransformerSized('readSequence'),
  readOption: readerTransformer('readOption'),
  readResult: readerTransformerTwoTyped('readResult'),
} as const
const dynBuilders = {
  Primitive: dynReader('Primitive'),
  Result: dynReader('Result'),
  Sequence: dynReader('Sequence'),
  Option: dynReader('Option'),
  Object: dynReader('Object'),
} as const

type ExpressionTransformer = (expression: ts.Expression) => ts.Expression

// === Public API ===

export class Type {
  readonly type: ts.TypeNode
  readonly reader: ExpressionTransformer
  readonly dynReader: ExpressionTransformer
  readonly size: number

  private constructor(
    type: ts.TypeNode,
    reader: ExpressionTransformer,
    dynReader: ExpressionTransformer,
    size: number,
  ) {
    this.type = type
    this.reader = reader
    this.dynReader = dynReader
    this.size = size
  }

  static Abstract(name: string): Type {
    const valueReader = abstractTypeReader(name)
    return new Type(
      tsf.createTypeReferenceNode(name),
      valueReader,
      dynBuilders.Object(valueReader),
      POINTER_SIZE,
    )
  }

  static Concrete(name: string, size: number): Type {
    const valueReader = concreteTypeReader(name)
    return new Type(
      tsf.createTypeReferenceNode(name),
      valueReader,
      dynBuilders.Object(valueReader),
      size,
    )
  }

  static Sequence(element: Type): Type {
    return new Type(
      tsf.createTypeReferenceNode('Iterable', [element.type]),
      cursorMethods.readSequence(element.reader, element.size),
      dynBuilders.Sequence(cursorMethods.readSequence(element.dynReader, element.size)),
      POINTER_SIZE,
    )
  }

  static Option(element: Type): Type {
    return new Type(
      tsf.createUnionTypeNode([element.type, noneType]),
      cursorMethods.readOption(element.reader),
      dynBuilders.Option(cursorMethods.readOption(element.dynReader)),
      POINTER_SIZE + 1,
    )
  }

  static Result(ok: Type, err: Type): Type {
    return new Type(
      support.Result(ok.type, err.type),
      cursorMethods.readResult(ok.reader, err.reader),
      dynBuilders.Result(cursorMethods.readResult(ok.dynReader, err.dynReader)),
      POINTER_SIZE,
    )
  }

  static Boolean: Type = new Type(
    tsf.createTypeReferenceNode('boolean'),
    cursorMethods.readBool,
    dynBuilders.Primitive(cursorMethods.readBool),
    1,
  )
  static UInt32: Type = new Type(
    tsf.createTypeReferenceNode('number'),
    cursorMethods.readU32,
    dynBuilders.Primitive(cursorMethods.readU32),
    4,
  )
  static Int32: Type = new Type(
    tsf.createTypeReferenceNode('number'),
    cursorMethods.readI32,
    dynBuilders.Primitive(cursorMethods.readI32),
    4,
  )
  static UInt64: Type = new Type(
    tsf.createTypeReferenceNode('bigint'),
    cursorMethods.readU64,
    dynBuilders.Primitive(cursorMethods.readU64),
    8,
  )
  static Int64: Type = new Type(
    tsf.createTypeReferenceNode('bigint'),
    cursorMethods.readI64,
    dynBuilders.Primitive(cursorMethods.readI64),
    8,
  )
  static Char: Type = new Type(
    tsf.createTypeReferenceNode('number'),
    cursorMethods.readU32,
    dynBuilders.Primitive(cursorMethods.readU32),
    4,
  )
  static String: Type = new Type(
    tsf.createTypeReferenceNode('string'),
    cursorMethods.readString,
    dynBuilders.Primitive(cursorMethods.readString),
    POINTER_SIZE,
  )
}

export function seekCursor(cursor: ts.Expression, offset: number): ts.Expression {
  if (offset === 0) {
    return cursor
  } else {
    return tsf.createCallExpression(
      tsf.createPropertyAccessExpression(cursor, 'seek'),
      [],
      [tsf.createNumericLiteral(offset)],
    )
  }
}

export function abstractTypeDeserializer(
  ident: ts.Identifier,
  cases: ts.CaseClause[],
): ts.FunctionDeclaration {
  const cursorIdent = tsf.createIdentifier('cursor')
  return tsf.createFunctionDeclaration(
    [modifiers.export],
    undefined,
    'read',
    [],
    [
      tsf.createParameterDeclaration(
        [],
        undefined,
        cursorIdent,
        undefined,
        support.Cursor,
        undefined,
      ),
    ],
    tsf.createTypeReferenceNode(ident),
    tsf.createBlock([
      tsf.createSwitchStatement(
        cursorMethods.readU32(cursorIdent),
        casesOrThrow(cases, 'Unexpected discriminant while deserializing.'),
      ),
    ]),
  )
}

export function fieldDeserializer(
  ident: ts.Identifier,
  type: Type,
  offset: number,
): ts.GetAccessorDeclaration {
  return tsf.createGetAccessorDeclaration(
    [],
    ident,
    [],
    type.type,
    tsf.createBlock([
      tsf.createReturnStatement(
        type.reader(
          seekCursor(
            tsf.createPropertyAccessExpression(tsf.createThis(), cursorFieldIdent),
            offset,
          ),
        ),
      ),
    ]),
  )
}

export function fieldDynValue(type: Type, offset: number): ts.Expression {
  return type.dynReader(
    seekCursor(tsf.createPropertyAccessExpression(tsf.createThis(), cursorFieldIdent), offset),
  )
}

// === Implementation ===

/** Returns a function that, given an expression evaluating to a [`Cursor`], returns an expression applying a
 * deserialization method with the given name to the cursor. */
function primitiveReader(name: string): ExpressionTransformer {
  return (cursor) =>
    tsf.createCallExpression(tsf.createPropertyAccessExpression(cursor, name), [], [])
}

/**
 * Given the name of a runtime `Cursor` method that deserializes a derived type given a function to deserialize a
 * base type, return a codegen-time function that generates a *reader* for a derived type from a *reader* for the base
 * type, where a *reader* is a function producing a deserialization expression from an expression that evaluates to a
 * `Cursor`.
 *
 * For example, if we have a reader produced by `primitiveReader('readU32')`, we can use it to create an expression
 * representing the deserialization of a number from an expression that will evaluate to a location in the input. If we
 * create a `readerTransformer('readOption')`, we can apply it to the number reader to yield an optional-number reader.
 */
function readerTransformer(
  name: string,
): (readElement: ExpressionTransformer) => ExpressionTransformer {
  const innerParameter = tsf.createIdentifier('element')
  return (readElement: ExpressionTransformer) => (cursor: ts.Expression) => {
    return tsf.createCallExpression(
      tsf.createPropertyAccessExpression(cursor, name),
      [],
      [
        tsf.createArrowFunction(
          [],
          [],
          [
            tsf.createParameterDeclaration(
              [],
              undefined,
              innerParameter,
              undefined,
              support.Cursor,
              undefined,
            ),
          ],
          undefined,
          undefined,
          readElement(innerParameter),
        ),
      ],
    )
  }
}

/** Similar to [`readerTransformer`], but for deserialization-transformers that produce a reader by combining two input
 * readers. */
function readerTransformerTwoTyped(
  name: string,
): (readOk: ExpressionTransformer, readErr: ExpressionTransformer) => ExpressionTransformer {
  function makeArrow(reader: ExpressionTransformer, data: ts.Identifier) {
    return tsf.createArrowFunction(
      [],
      [],
      [tsf.createParameterDeclaration([], undefined, data, undefined, support.Cursor, undefined)],
      undefined,
      undefined,
      reader(data),
    )
  }

  const okData = tsf.createIdentifier('okData')
  const errData = tsf.createIdentifier('errData')
  return (readOk: ExpressionTransformer, readErr: ExpressionTransformer) =>
    (cursor: ts.Expression) => {
      return tsf.createCallExpression(
        tsf.createPropertyAccessExpression(cursor, name),
        [],
        [makeArrow(readOk, okData), makeArrow(readErr, errData)],
      )
    }
}

/** Similar to [`readerTransformer`], but for deserialization-transformers are parameterized by the size of their
 * element. */
function readerTransformerSized(
  name: string,
): (readElement: ExpressionTransformer, size: number) => ExpressionTransformer {
  const innerParameter = tsf.createIdentifier('element')
  return (readElement: ExpressionTransformer, size: number) => (cursor: ts.Expression) => {
    return tsf.createCallExpression(
      tsf.createPropertyAccessExpression(cursor, name),
      [],
      [
        tsf.createArrowFunction(
          [],
          [],
          [
            tsf.createParameterDeclaration(
              [],
              undefined,
              innerParameter,
              undefined,
              support.Cursor,
              undefined,
            ),
          ],
          undefined,
          undefined,
          readElement(innerParameter),
        ),
        tsf.createNumericLiteral(size),
      ],
    )
  }
}

function dynReader(name: string): (readValue: ExpressionTransformer) => ExpressionTransformer {
  return (readValue: ExpressionTransformer) => (cursor: ts.Expression) => {
    return tsf.createCallExpression(
      tsf.createPropertyAccessExpression(support.Dyn, name),
      [],
      [readValue(cursor)],
    )
  }
}

function abstractTypeReader(name: string): ExpressionTransformer {
  return (cursor: ts.Expression) =>
    tsf.createCallExpression(
      tsf.createPropertyAccessExpression(tsf.createIdentifier(name), 'read'),
      [],
      [cursorMethods.readPointer(cursor)],
    )
}

function concreteTypeReader(name: string): ExpressionTransformer {
  return (cursor: ts.Expression) =>
    tsf.createCallExpression(
      tsf.createPropertyAccessExpression(tsf.createIdentifier(name), 'read'),
      [],
      [cursor],
    )
}
