import * as fs from 'fs'
import * as ts from 'typescript'
import { factory as tsf } from 'typescript'

module Schema {
  export type Type = {
    name: string
    fields: [string, Field][]
    parent: string | null
    discriminants: [number: string]
    size: number
  }
  export type Field = {
    type: TypeRef
    offset: number
  }
  export type TypeRef = Class | Primitive | Sequence | Option | Result
  export type Class = { class: 'type'; id: string }
  export type Primitive = { class: 'primitive'; type: PrimitiveType }
  export type Sequence = { class: 'sequence'; type: TypeRef }
  export type Option = { class: 'option'; type: TypeRef }
  export type Result = { class: 'result'; type0: TypeRef; type1: TypeRef }
  export type PrimitiveType = 'bool' | 'u32' | 'u64' | 'i32' | 'i64' | 'char' | 'string'
}
type TypeGraph = {
  [id: string]: Schema.Type
}
type Schema = {
  types: TypeGraph
}

function fromSnake(ident: string, to: 'camel' | 'pascal', prefix?: string): string {
  const segments = []
  if (prefix !== undefined) {
    segments.push(...prefix.split('_'))
  }
  segments.push(...ident.split('_'))
  return segments
    .map((seg, i) => {
      if (to === 'camel' && i === 0) {
        return seg
      } else {
        return seg.charAt(0).toUpperCase() + seg.slice(1)
      }
    })
    .join('')
}

function toPascal(ident: string, prefix?: string): string {
  return fromSnake(ident, 'pascal', prefix)
}

function toCamel(ident: string, prefix?: string): string {
  return fromSnake(ident, 'camel', prefix)
}

function legalizeIdent(ident: string): string {
  // FIXME: We should accept a renaming table as an input alongside the schema, then emit an error if a keyword is
  //  encountered ("constructor") or a field name is duplicated ("type").
  switch (ident) {
    case 'constructor':
      return 'ident'
    case 'type':
      return 'typeNode'
    default:
      return ident
  }
}

type ExpressionTransformer = (expression: ts.Expression) => ts.Expression

function primitiveReader(name: string): ExpressionTransformer {
  return (cursor) =>
    tsf.createCallExpression(tsf.createPropertyAccessExpression(cursor, name), [], [])
}

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

function readerTransformer2(
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

function namespacedName(name: string, namespace?: string): string {
  if (namespace === undefined) {
    return toPascal(name)
  } else {
    return toPascal(namespace) + '.' + toPascal(name)
  }
}

function abstractTypeReader(name: string): ExpressionTransformer {
  return (cursor: ts.Expression) =>
    tsf.createCallExpression(
      tsf.createPropertyAccessExpression(tsf.createIdentifier(toPascal(name)), 'read'),
      [],
      [cursorMethods.readPointer(cursor)],
    )
}

function concreteTypeReader(name: string): ExpressionTransformer {
  return (cursor: ts.Expression) =>
    tsf.createCallExpression(
      tsf.createPropertyAccessExpression(tsf.createIdentifier(toPascal(name)), 'read'),
      [],
      [cursor],
    )
}

class Type {
  readonly type: ts.TypeNode
  readonly reader: ExpressionTransformer
  readonly size: number

  constructor(type: ts.TypeNode, reader: ExpressionTransformer, size: number) {
    this.type = type
    this.reader = reader
    this.size = size
  }

  static new(ref: Schema.TypeRef, types: TypeGraph): Type {
    const c = ref.class
    switch (c) {
      case 'type':
        const ty = types[ref.id]
        const parentId = ty.parent
        const parent = ty.parent != null ? types[ty.parent] : null
        const typeName = namespacedName(ty.name, parent?.name)
        const type = tsf.createTypeReferenceNode(typeName)
        if (Object.entries(ty.discriminants).length !== 0) {
          return new Type(type, abstractTypeReader(typeName), POINTER_SIZE)
        } else {
          return new Type(type, concreteTypeReader(typeName), ty.size)
        }
      case 'primitive':
        const p = ref.type
        switch (p) {
          case 'bool':
            return new Type(tsf.createTypeReferenceNode('boolean'), cursorMethods.readBool, 1)
          case 'u32':
            return new Type(tsf.createTypeReferenceNode('number'), cursorMethods.readU32, 4)
          case 'i32':
            return new Type(tsf.createTypeReferenceNode('number'), cursorMethods.readI32, 4)
          case 'u64':
            return new Type(tsf.createTypeReferenceNode('number'), cursorMethods.readU64, 8)
          case 'i64':
            return new Type(tsf.createTypeReferenceNode('number'), cursorMethods.readI64, 8)
          case 'char':
            return new Type(tsf.createTypeReferenceNode('number'), cursorMethods.readU32, 4)
          case 'string':
            return new Type(
              tsf.createTypeReferenceNode('string'),
              cursorMethods.readString,
              POINTER_SIZE,
            )
          default:
            const _ = p satisfies never
            throw new Error("unreachable: PrimitiveType.type='" + p + "'")
        }
      case 'sequence':
        return Type.sequence(Type.new(ref.type, types))
      case 'option':
        return Type.option(Type.new(ref.type, types))
      case 'result':
        return Type.result(Type.new(ref.type0, types), Type.new(ref.type1, types))
      default:
        const _ = c satisfies never
        throw new Error("unreachable: TypeRef.class='" + c + "' in " + JSON.stringify(ref))
    }
  }

  static sequence(element: Type): Type {
    return new Type(
      tsf.createTypeReferenceNode('Iterable', [element.type]),
      cursorMethods.readSequence(element.reader, element.size),
      POINTER_SIZE,
    )
  }

  static option(element: Type): Type {
    return new Type(
      tsf.createUnionTypeNode([element.type, nullType]),
      cursorMethods.readOption(element.reader),
      POINTER_SIZE + 1,
    )
  }

  static result(ok: Type, err: Type): Type {
    return new Type(
      tsf.createUnionTypeNode([ok.type, err.type]),
      cursorMethods.readResult(ok.reader, err.reader),
      POINTER_SIZE,
    )
  }
}

function seekCursor(cursor: ts.Expression, offset: number): ts.Expression {
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

function makeGetter(
  fieldName: string,
  fieldData: Schema.Field,
  types: TypeGraph,
): ts.GetAccessorDeclaration {
  const type = Type.new(fieldData.type, types)
  return tsf.createGetAccessorDeclaration(
    [],
    tsf.createIdentifier(legalizeIdent(toCamel(fieldName))),
    [],
    type.type,
    tsf.createBlock([
      tsf.createReturnStatement(
        type.reader(
          seekCursor(
            tsf.createPropertyAccessExpression(tsf.createThis(), cursorFieldIdent),
            fieldData.offset,
          ),
        ),
      ),
    ]),
  )
}

// Helper for a common case of constructing an assignment.
function createAssignmentStatement(left: ts.Expression, right: ts.Expression): ts.Statement {
  return tsf.createExpressionStatement(
    tsf.createBinaryExpression(left, ts.SyntaxKind.EqualsToken, right),
  )
}

function makeConcreteType(id: string, types: TypeGraph): ts.ClassDeclaration {
  const ident = tsf.createIdentifier(toPascal(types[id].name))
  const paramIdent = tsf.createIdentifier('cursor')
  const cursorParam = tsf.createParameterDeclaration(
    [],
    undefined,
    paramIdent,
    undefined,
    support.Cursor,
    undefined,
  )
  return makeClass(
    [modifiers.export],
    ident,
    [
      forwardToSuper(paramIdent, support.Cursor),
      tsf.createMethodDeclaration(
        [modifiers.static],
        undefined,
        'read',
        undefined,
        [],
        [cursorParam],
        tsf.createTypeReferenceNode(ident),
        tsf.createBlock([
          tsf.createReturnStatement(tsf.createNewExpression(ident, [], [paramIdent])),
        ]),
      ),
    ],
    id,
    types,
  )
}

function debugValue(ident: ts.Identifier): ts.Expression {
  const value = tsf.createPropertyAccessExpression(tsf.createThis(), ident)
  return tsf.createCallExpression(support.debugHelper, [], [value])
}

function makeDebugFunction(fields: [string, Schema.Field][]): ts.MethodDeclaration {
  const getterIdent = (fieldName: string) => tsf.createIdentifier(legalizeIdent(toCamel(fieldName)))
  return tsf.createMethodDeclaration(
    [],
    undefined,
    'debug',
    undefined,
    [],
    [],
    tsf.createTypeReferenceNode('any'),
    tsf.createBlock([
      tsf.createReturnStatement(
        tsf.createObjectLiteralExpression([
          tsf.createSpreadAssignment(
            tsf.createCallExpression(
              tsf.createPropertyAccessExpression(tsf.createSuper(), 'debug'),
              [],
              [],
            ),
          ),
          ...fields.map(([name, field]: [string, Schema.Field]) =>
            tsf.createPropertyAssignment(getterIdent(name), debugValue(getterIdent(name))),
          ),
        ]),
      ),
    ]),
  )
}

function makeClass(
  modifiers: ts.Modifier[],
  name: ts.Identifier,
  members: ts.ClassElement[],
  id: string,
  types: TypeGraph,
): ts.ClassDeclaration {
  const ty = types[id]
  return tsf.createClassDeclaration(
    modifiers,
    name,
    undefined,
    [
      tsf.createHeritageClause(ts.SyntaxKind.ExtendsKeyword, [
        tsf.createExpressionWithTypeArguments(support.LazyObject, []),
      ]),
    ],
    [
      ...members,
      ...ty.fields.map(([name, field]: [string, Schema.Field]) => makeGetter(name, field, types)),
      makeDebugFunction(ty.fields),
    ],
  )
}

type ChildType = {
  definition: ts.ClassDeclaration
  reference: ts.TypeNode
  enumMember: ts.EnumMember
  case: ts.CaseClause
}

function makeChildType(
  parentName: string,
  base: ts.Identifier,
  id: string,
  discrim: string,
  types: TypeGraph,
): ChildType {
  const ty: Schema.Type = types[id]
  const name = toPascal(ty.name)
  const ident = tsf.createIdentifier(name)
  const cursorIdent = tsf.createIdentifier('cursor')
  const cursorParam = tsf.createParameterDeclaration(
    [],
    undefined,
    cursorIdent,
    undefined,
    support.Cursor,
    undefined,
  )
  const discrimInt = tsf.createNumericLiteral(parseInt(discrim, 10))
  return {
    definition: tsf.createClassDeclaration(
      [modifiers.export],
      name,
      undefined,
      [
        tsf.createHeritageClause(ts.SyntaxKind.ExtendsKeyword, [
          tsf.createExpressionWithTypeArguments(base, []),
        ]),
      ],
      [
        tsf.createPropertyDeclaration(
          [modifiers.readonly],
          'type',
          undefined,
          tsf.createTypeReferenceNode('Type.' + name),
          undefined,
        ),
        tsf.createConstructorDeclaration(
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
          tsf.createBlock([
            tsf.createExpressionStatement(
              tsf.createCallExpression(tsf.createIdentifier('super'), [], [cursorIdent]),
            ),
            createAssignmentStatement(
              tsf.createPropertyAccessExpression(tsf.createIdentifier('this'), 'type'),
              tsf.createPropertyAccessExpression(tsf.createIdentifier('Type'), name),
            ),
          ]),
        ),
        tsf.createMethodDeclaration(
          [modifiers.static],
          undefined,
          'read',
          undefined,
          [],
          [cursorParam],
          tsf.createTypeReferenceNode(ident),
          tsf.createBlock([
            tsf.createReturnStatement(tsf.createNewExpression(ident, [], [cursorIdent])),
          ]),
        ),
        ...ty.fields.map(([name, field]: [string, Schema.Field]) => makeGetter(name, field, types)),
        makeDebugFunction(ty.fields),
      ],
    ),
    reference: tsf.createTypeReferenceNode(name),
    enumMember: tsf.createEnumMember(toPascal(types[id].name), discrimInt),
    case: tsf.createCaseClause(discrimInt, [
      tsf.createReturnStatement(tsf.createNewExpression(ident, [], [seekCursor(cursorIdent, 4)])),
    ]),
  }
}

function forwardToSuper(ident: ts.Identifier, type: ts.TypeNode) {
  return tsf.createConstructorDeclaration(
    [],
    [tsf.createParameterDeclaration([], undefined, ident, undefined, type, undefined)],
    tsf.createBlock([
      tsf.createExpressionStatement(
        tsf.createCallExpression(tsf.createIdentifier('super'), [], [ident]),
      ),
    ]),
  )
}

function casesOrThrow(cases: ts.CaseClause[], error: string): ts.CaseBlock {
  return tsf.createCaseBlock([
    ...cases,
    tsf.createDefaultClause([
      tsf.createThrowStatement(
        tsf.createNewExpression(
          tsf.createIdentifier('Error'),
          [],
          [tsf.createStringLiteral(error)],
        ),
      ),
    ]),
  ])
}

function abstractTypeDeserializer(
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

function makeAbstractType(id: string, types: TypeGraph) {
  const ty = types[id]
  const name = toPascal(ty.name)
  const ident = tsf.createIdentifier(name)
  const baseIdent = tsf.createIdentifier('AbstractBase')
  const childTypes = Array.from(
    Object.entries(ty.discriminants),
    ([discrim, id]: [string, string]) => makeChildType(name, baseIdent, id, discrim, types),
  )
  const cursorIdent = tsf.createIdentifier('cursor')
  const moduleDecl = tsf.createModuleDeclaration(
    [modifiers.export],
    ident,
    tsf.createModuleBlock([
      makeClass(
        [modifiers.abstract],
        baseIdent,
        [forwardToSuper(cursorIdent, support.Cursor)],
        id,
        types,
      ),
      tsf.createEnumDeclaration(
        [modifiers.export, modifiers.const],
        'Type',
        childTypes.map((child) => child.enumMember),
      ),
      ...childTypes.map((child) => child.definition),
      tsf.createTypeAliasDeclaration(
        [modifiers.export],
        ident,
        undefined,
        tsf.createUnionTypeNode(childTypes.map((child) => child.reference)),
      ),
      abstractTypeDeserializer(
        ident,
        childTypes.map((child) => child.case),
      ),
    ]),
  )
  const abstractTypeExport = tsf.createTypeAliasDeclaration(
    [modifiers.export],
    ident,
    undefined,
    tsf.createTypeReferenceNode(name + '.' + name),
  )
  emit(moduleDecl)
  emit(abstractTypeExport)
}

function emit(data: ts.Node) {
  output += printer.printNode(ts.EmitHint.Unspecified, data, file)
  output += '\n'
}

// ============
// === Main ===
// ============

const data: Schema = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'))
let output = '// *** THIS FILE GENERATED BY `parser-codegen` ***\n'
const file = ts.createSourceFile('source.ts', '', ts.ScriptTarget.ESNext, false, ts.ScriptKind.TS)
const printer = ts.createPrinter({ newLine: ts.NewLineKind.LineFeed })
const nullType = tsf.createTypeReferenceNode('null')
const cursorFieldIdent = tsf.createIdentifier('lazyObjectData')
const modifiers = {
  export: tsf.createModifier(ts.SyntaxKind.ExportKeyword),
  const: tsf.createModifier(ts.SyntaxKind.ConstKeyword),
  readonly: tsf.createModifier(ts.SyntaxKind.ReadonlyKeyword),
  abstract: tsf.createModifier(ts.SyntaxKind.AbstractKeyword),
  static: tsf.createModifier(ts.SyntaxKind.StaticKeyword),
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
  readResult: readerTransformer2('readResult'),
} as const
const POINTER_SIZE: number = 4
// Symbols exported by the `parserSupport` module.
const support = {
  LazyObject: tsf.createIdentifier('LazyObject'),
  Cursor: tsf.createTypeReferenceNode(tsf.createIdentifier('Cursor')),
  debugHelper: tsf.createIdentifier('debugHelper'),
} as const

emit(
  tsf.createImportDeclaration(
    [],
    tsf.createImportClause(
      false,
      undefined,
      tsf.createNamedImports(
        Array.from(Object.entries(support), ([name, _value]) =>
          tsf.createImportSpecifier(false, undefined, tsf.createIdentifier(name)),
        ),
      ),
    ),
    tsf.createStringLiteral('@/util/parserSupport', true),
    undefined,
  ),
)
for (const id in data.types) {
  const ty = data.types[id]
  if (ty.parent === null) {
    if (Object.entries(data.types[id].discriminants).length === 0) {
      emit(makeConcreteType(id, data.types))
    } else {
      makeAbstractType(id, data.types)
    }
  } else {
    // Ignore child types; they are generated when `makeAbstractType` processes the parent.
  }
}
output += `export function deserializeTree(data: ArrayBuffer): Tree {
  const cursor = new Cursor(data, data.byteLength - 4)
  return Tree.read(cursor.readPointer())
}`
fs.writeFileSync(process.argv[3], output)
