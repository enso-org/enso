/** Generates code lazily deserializing from an application-specific binary format. */

import ts from 'typescript'
import { makeArrow } from './util'

const { factory: tsf } = ts

// === Definitions ===

const noneType = tsf.createTypeReferenceNode('undefined')
const viewFieldIdent = tsf.createIdentifier('_v')
const variantReadersIdent = tsf.createIdentifier('VARIANT_READERS')
const POINTER_SIZE: number = 4
// Symbols exported by the `parserSupport` module.
export const supportImports = {
  LazyObject: false,
  Result: true,
  DynValue: true,
  Dyn: false,
  readU8: false,
  readU32: false,
  readI32: false,
  readU64: false,
  readI64: false,
  readBool: false,
  readOffset: false,
  readPointer: false,
  readOption: false,
  readResult: false,
  readEnum: false,
  readSequence: false,
  readString: false,
} as const
export const support = {
  LazyObject: tsf.createIdentifier('LazyObject'),
  DataView: tsf.createTypeReferenceNode(tsf.createIdentifier('DataView')),
  Result: (t0: ts.TypeNode, t1: ts.TypeNode) =>
    tsf.createTypeReferenceNode(tsf.createIdentifier('Result'), [t0, t1]),
  DynValue: 'DynValue',
  Dyn: tsf.createIdentifier('Dyn'),
  readU8: tsf.createIdentifier('readU8'),
  readU32: tsf.createIdentifier('readU32'),
  readI32: tsf.createIdentifier('readI32'),
  readU64: tsf.createIdentifier('readU64'),
  readI64: tsf.createIdentifier('readI64'),
  readBool: tsf.createIdentifier('readBool'),
  readOffset: tsf.createIdentifier('readOffset'),
  readPointer: tsf.createIdentifier('readPointer'),
  readOption: tsf.createIdentifier('readOption'),
  readResult: tsf.createIdentifier('readResult'),
  readEnum: tsf.createIdentifier('readEnum'),
  readSequence: tsf.createIdentifier('readSequence'),
  readString: tsf.createIdentifier('readString'),
} as const

const baseReaders = {
  readString: primitiveReader(support.readString),
  readBool: primitiveReader(support.readBool),
  readU32: primitiveReader(support.readU32),
  readI32: primitiveReader(support.readI32),
  readU64: primitiveReader(support.readU64),
  readI64: primitiveReader(support.readI64),
  readPointer: primitiveReader(support.readPointer),
  readOffset: primitiveReader(support.readOffset),
  readOption: readerTransformer(support.readOption),
  readResult: readerTransformerTwoTyped(support.readResult),
} as const
const dynBuilders = {
  Primitive: dynReader('Primitive'),
  Result: dynReader('Result'),
  Sequence: dynReader('Sequence'),
  Option: dynReader('Option'),
  Object: dynReader('Object'),
} as const

type ReadApplicator = (cursor: ts.Expression, offset: AccessOffset) => ts.Expression

// === Public API ===

export class Type {
  readonly type: ts.TypeNode
  readonly reader: ReadApplicator
  readonly dynReader: ReadApplicator
  readonly size: number

  private constructor(
    type: ts.TypeNode,
    reader: ReadApplicator,
    dynReader: ReadApplicator,
    size: number,
  ) {
    this.type = type
    this.reader = reader
    this.dynReader = dynReader
    this.size = size
  }

  static Abstract(name: string): Type {
    const valueReader = callRead(name)
    return new Type(
      tsf.createTypeReferenceNode(name),
      valueReader,
      dynBuilders.Object(valueReader),
      POINTER_SIZE,
    )
  }

  static Concrete(name: string, size: number): Type {
    const valueReader = callRead(name)
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
      createSequenceReader(element.size, element.reader),
      dynBuilders.Sequence(createSequenceReader(element.size, element.dynReader)),
      POINTER_SIZE,
    )
  }

  static Option(element: Type): Type {
    return new Type(
      tsf.createUnionTypeNode([element.type, noneType]),
      baseReaders.readOption(element.reader),
      dynBuilders.Option(baseReaders.readOption(element.dynReader)),
      POINTER_SIZE + 1,
    )
  }

  static Result(ok: Type, err: Type): Type {
    return new Type(
      support.Result(ok.type, err.type),
      baseReaders.readResult(ok.reader, err.reader),
      dynBuilders.Result(baseReaders.readResult(ok.dynReader, err.dynReader)),
      POINTER_SIZE,
    )
  }

  static Boolean: Type = new Type(
    tsf.createTypeReferenceNode('boolean'),
    baseReaders.readBool,
    dynBuilders.Primitive(baseReaders.readBool),
    1,
  )
  static UInt32: Type = new Type(
    tsf.createTypeReferenceNode('number'),
    baseReaders.readU32,
    dynBuilders.Primitive(baseReaders.readU32),
    4,
  )
  static Int32: Type = new Type(
    tsf.createTypeReferenceNode('number'),
    baseReaders.readI32,
    dynBuilders.Primitive(baseReaders.readI32),
    4,
  )
  static UInt64: Type = new Type(
    tsf.createTypeReferenceNode('bigint'),
    baseReaders.readU64,
    dynBuilders.Primitive(baseReaders.readU64),
    8,
  )
  static Int64: Type = new Type(
    tsf.createTypeReferenceNode('bigint'),
    baseReaders.readI64,
    dynBuilders.Primitive(baseReaders.readI64),
    8,
  )
  static Char: Type = new Type(
    tsf.createTypeReferenceNode('number'),
    baseReaders.readU32,
    dynBuilders.Primitive(baseReaders.readU32),
    4,
  )
  static String: Type = new Type(
    tsf.createTypeReferenceNode('string'),
    baseReaders.readString,
    dynBuilders.Primitive(baseReaders.readString),
    POINTER_SIZE,
  )
}

export function seekView(view: ts.Expression, address: number): ts.Expression {
  if (address === 0) {
    return view
  } else {
    return seekViewDyn(view, tsf.createNumericLiteral(address))
  }
}

export function seekViewDyn(view: ts.Expression, address: ts.Expression): ts.Expression {
  return tsf.createCallExpression(support.readOffset, [], [view, address])
}

export function abstractTypeVariants(cases: ts.Identifier[]): ts.Statement {
  const reads = cases.map((c) => tsf.createPropertyAccessChain(c, undefined, 'read'))
  return tsf.createVariableStatement(
    [],
    tsf.createVariableDeclarationList(
      [
        tsf.createVariableDeclaration(
          variantReadersIdent,
          undefined,
          undefined,
          tsf.createArrayLiteralExpression(reads),
        ),
      ],
      ts.NodeFlags.Const,
    ),
  )
}

export function abstractTypeDeserializer(
  ident: ts.Identifier,
  cursorIdent: ts.Identifier,
  offsetIdent: ts.Identifier,
): ts.Expression {
  return tsf.createCallExpression(
    support.readEnum,
    [tsf.createTypeReferenceNode(ident)],
    [variantReadersIdent, cursorIdent, offsetIdent],
  )
}

export function fieldDeserializer(
  ident: ts.Identifier,
  type: Type,
  address: number,
): ts.GetAccessorDeclaration {
  return tsf.createGetAccessorDeclaration(
    [],
    ident,
    [],
    type.type,
    tsf.createBlock([
      tsf.createReturnStatement(
        type.reader(thisAccess(viewFieldIdent), makeConstantAddress(address)),
      ),
    ]),
  )
}

export function fieldDynValue(type: Type, address: number): ts.Expression {
  return type.dynReader(thisAccess(viewFieldIdent), makeConstantAddress(address))
}

function thisAccess(ident: ts.Identifier): ts.PropertyAccessExpression {
  return tsf.createPropertyAccessExpression(tsf.createThis(), ident)
}

// === Implementation ===

/** Returns a function that, given an expression evaluating to a [`Cursor`], returns an expression applying a
 * deserialization method with the given name to the cursor. */
function primitiveReader(func: ts.Identifier): ReadApplicator {
  return (view, address) => tsf.createCallExpression(func, [], [view, materializeAddress(address)])
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
function readerTransformer(func: ts.Identifier): (readElement: ReadApplicator) => ReadApplicator {
  return (readElement) => (view, offset) => {
    return tsf.createCallExpression(
      func,
      [],
      [view, materializeAddress(offset), readerClosure(readElement)],
    )
  }
}

interface AccessOffset {
  expression: ts.Expression | null
  constant: number
}

function makeConstantAddress(constant: number): AccessOffset {
  return { expression: null, constant }
}

function makeDynAddress(expression: ts.Expression, constant = 0): AccessOffset {
  return { expression, constant }
}

function materializeAddress(offset: AccessOffset): ts.Expression {
  if (offset.expression == null) {
    return tsf.createNumericLiteral(offset.constant)
  } else if (offset.constant == 0) {
    return offset.expression
  } else {
    return tsf.createAdd(offset.expression, tsf.createNumericLiteral(offset.constant))
  }
}

/** Similar to [`readerTransformer`], but for deserialization-transformers that produce a reader by combining two input
 * readers. */
function readerTransformerTwoTyped(
  func: ts.Identifier,
): (readOk: ReadApplicator, readErr: ReadApplicator) => ReadApplicator {
  return (readOk: ReadApplicator, readErr: ReadApplicator) => (view, offset) => {
    return tsf.createCallExpression(
      func,
      [],
      [view, materializeAddress(offset), readerClosure(readOk), readerClosure(readErr)],
    )
  }
}

function dynReader(name: string): (readValue: ReadApplicator) => ReadApplicator {
  return (readValue) => (view, address) => {
    return tsf.createCallExpression(
      tsf.createPropertyAccessExpression(support.Dyn, name),
      [],
      [readValue(view, address)],
    )
  }
}

export function callRead(ident: string): ReadApplicator {
  return (view, address) =>
    tsf.createCallExpression(
      tsf.createPropertyAccessExpression(tsf.createIdentifier(ident), 'read'),
      [],
      [view, materializeAddress(address)],
    )
}

export function createSequenceReader(size: number, reader: ReadApplicator): ReadApplicator {
  const sizeLiteral = tsf.createNumericLiteral(size)
  const closure = readerClosure(reader)
  return (view, address) =>
    tsf.createCallExpression(
      support.readSequence,
      [],
      [view, materializeAddress(address), sizeLiteral, closure],
    )
}

export function readerClosure(reader: ReadApplicator): ts.Expression {
  const view = tsf.createIdentifier('view')
  const address = tsf.createIdentifier('address')
  const read = reader(view, makeDynAddress(address))
  if (isSimpleRead(read)) {
    return read.expression
  } else {
    return makeArrow([view, address], read)
  }
}

function isSimpleRead(reader: ts.Expression): reader is ts.CallExpression {
  return (
    ts.isCallExpression(reader) &&
    ts.isPropertyAccessExpression(reader.expression) &&
    reader.expression.name.text === 'read' &&
    reader.arguments.length === 2
  )
}

function dbg<T extends ts.Node | undefined>(node: T): T {
  if (node == null) {
    console.log(node)
    return node
  }
  const printer = ts.createPrinter({
    newLine: ts.NewLineKind.LineFeed,
    omitTrailingSemicolon: true,
  })

  console.log(
    ts.SyntaxKind[node.kind],
    ':',
    printer.printNode(
      ts.EmitHint.Unspecified,
      node,
      ts.createSourceFile('dbg.ts', '', ts.ScriptTarget.Latest, false, ts.ScriptKind.TS),
    ),
  )
  return node
}
