import * as fs from 'fs'
import ts from 'typescript'
const { factory: tsf } = ts
import * as changeCase from 'change-case'

module Schema {
  export type TypeId = string
  export type Types = {
    [id: TypeId]: Type
  }
  export type Type = {
    name: string
    fields: Fields
    parent?: string
  }
  export type Fields = {
    [name: string]: TypeRef
  }
  export type TypeRef = Class | Primitive | Sequence | Option | Result
  export type Class = { class: 'type'; id: TypeId }
  export type Primitive = { class: 'primitive'; type: PrimitiveType }
  export type Sequence = { class: 'sequence'; type: TypeRef }
  export type Option = { class: 'option'; type: TypeRef }
  export type Result = { class: 'result'; type0: TypeRef; type1: TypeRef }
  export type PrimitiveType = 'bool' | 'u32' | 'u64' | 'i32' | 'i64' | 'char' | 'string'

  export type Serialization = {
    [id: TypeId]: Layout
  }
  export type Layout = {
    discriminants?: DiscriminantMap
    fields: [name: string, offset: number][]
    size: number
  }
  export type DiscriminantMap = {
    [discriminant: number]: TypeId
  }
}
type Schema = {
  types: Schema.Types
  serialization: Schema.Serialization
}

function toPascal(ident: string): string {
  if (ident.includes('.')) throw new Error("toPascal cannot be applied to a namespaced name.")
  return changeCase.pascalCase(ident)
}

function toCamel(ident: string): string {
  if (ident.includes('.')) throw new Error("toCamel cannot be applied to a namespaced name.")
  return changeCase.camelCase(ident)
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

/**
 * Given the name of a runtime `Cursor` method that deserializes a derived type given a function to deserialize a
 * base type, return a codegen-time function that generates a *reader* for a derived type from a *reader* for the base
 * type, where a *reader* is a function producing a deserialization expression from an expression that evaluates to a
 * `Cursor`.
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
  if (namespace == null) {
    return toPascal(name)
  } else {
    return toPascal(namespace) + '.' + toPascal(name)
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

class Type {
  readonly type: ts.TypeNode
  readonly reader: ExpressionTransformer
  readonly size: number

  constructor(type: ts.TypeNode, reader: ExpressionTransformer, size: number) {
    this.type = type
    this.reader = reader
    this.size = size
  }

  static new(ref: Schema.TypeRef, schema: Schema): Type {
    const c = ref.class
    switch (c) {
      case 'type':
        const ty = schema.types[ref.id]
        const parent = ty.parent != null ? schema.types[ty.parent] : undefined
        const typeName = namespacedName(ty.name, parent?.name)
        const type = tsf.createTypeReferenceNode(typeName)
        const layout = schema.serialization[ref.id]
        if (layout.discriminants != null) {
          return new Type(type, abstractTypeReader(typeName), POINTER_SIZE)
        } else {
          return new Type(type, concreteTypeReader(typeName), layout.size)
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
            return new Type(tsf.createTypeReferenceNode('bigint'), cursorMethods.readU64, 8)
          case 'i64':
            return new Type(tsf.createTypeReferenceNode('bigint'), cursorMethods.readI64, 8)
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
        return Type.sequence(Type.new(ref.type, schema))
      case 'option':
        return Type.option(Type.new(ref.type, schema))
      case 'result':
        return Type.result(Type.new(ref.type0, schema), Type.new(ref.type1, schema))
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
      tsf.createUnionTypeNode([element.type, noneType]),
      cursorMethods.readOption(element.reader),
      POINTER_SIZE + 1,
    )
  }

  static result(ok: Type, err: Type): Type {
    return new Type(
      support.Result(ok.type, err.type),
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
  typeRef: Schema.TypeRef,
  offset: number,
  schema: Schema,
): ts.GetAccessorDeclaration {
  const type = Type.new(typeRef, schema)
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
            offset,
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

function makeConcreteType(id: string, schema: Schema): ts.ClassDeclaration {
  const ident = tsf.createIdentifier(toPascal(schema.types[id].name))
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
    schema,
  )
}

function debugValue(ident: ts.Identifier): ts.Expression {
  const value = tsf.createPropertyAccessExpression(tsf.createThis(), ident)
  return tsf.createCallExpression(support.debugHelper, [], [value])
}

function makeDebugFunction(fields: string[]): ts.MethodDeclaration {
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
        tsf.createCallExpression(support.debugHelper, [], [
          tsf.createObjectLiteralExpression([
            tsf.createSpreadAssignment(
              tsf.createCallExpression(
                tsf.createPropertyAccessExpression(tsf.createSuper(), 'debug'),
                [],
                [],
              ),
            ),
            ...fields.map((name: string) =>
              tsf.createPropertyAssignment(getterIdent(name), debugValue(getterIdent(name))),
            ),
          ])
        ]),
      ),
    ]),
  )
}

function makeGetters(id: string, schema: Schema): ts.ClassElement[] {
  return schema.serialization[id].fields.map(([name, offset]: [string, number]) =>
    makeGetter(name, schema.types[id].fields[name], offset, schema))
}

function makeClass(
  modifiers: ts.Modifier[],
  name: ts.Identifier,
  members: ts.ClassElement[],
  id: string,
  schema: Schema,
): ts.ClassDeclaration {
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
      ...makeGetters(id, schema),
      makeDebugFunction(Object.getOwnPropertyNames(schema.types[id].fields)),
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
  base: ts.Identifier,
  id: string,
  discrim: string,
  schema: Schema,
): ChildType {
  const ty: Schema.Type = schema.types[id]
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
        ...makeGetters(id, schema),
        makeDebugFunction(Object.getOwnPropertyNames(ty.fields)),
      ],
    ),
    reference: tsf.createTypeReferenceNode(name),
    enumMember: tsf.createEnumMember(toPascal(schema.types[id].name), discrimInt),
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

function makeAbstractType(id: string, discriminants: Schema.DiscriminantMap, schema: Schema) {
  const ty = schema.types[id]
  const name = toPascal(ty.name)
  const ident = tsf.createIdentifier(name)
  const baseIdent = tsf.createIdentifier('AbstractBase')
  const childTypes = Array.from(
    Object.entries(discriminants),
    ([discrim, id]: [string, string]) => makeChildType(baseIdent, id, discrim, schema),
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
        schema,
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

const schema: Schema = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'))
let output = '// *** THIS FILE GENERATED BY `parser-codegen` ***\n'
const file = ts.createSourceFile('source.ts', '', ts.ScriptTarget.ESNext, false, ts.ScriptKind.TS)
const printer = ts.createPrinter({ newLine: ts.NewLineKind.LineFeed })
const noneType = tsf.createTypeReferenceNode('undefined')
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
  readResult: readerTransformerTwoTyped('readResult'),
} as const
const POINTER_SIZE: number = 4
// Symbols exported by the `parserSupport` module.
const supportImports = {
  LazyObject: false,
  Cursor: false,
  debugHelper: false,
  Result: true,
} as const
const support = {
  LazyObject: tsf.createIdentifier('LazyObject'),
  Cursor: tsf.createTypeReferenceNode(tsf.createIdentifier('Cursor')),
  debugHelper: tsf.createIdentifier('debugHelper'),
  Result: (t0: ts.TypeNode, t1: ts.TypeNode) =>
    tsf.createTypeReferenceNode(tsf.createIdentifier('Result'), [t0, t1]),
} as const
emit(
  tsf.createImportDeclaration(
    [],
    tsf.createImportClause(
      false,
      undefined,
      tsf.createNamedImports(
        Array.from(Object.entries(supportImports), ([name, isTypeOnly]) =>
          tsf.createImportSpecifier(isTypeOnly, undefined, tsf.createIdentifier(name)),
        ),
      ),
    ),
    tsf.createStringLiteral('@/util/parserSupport', true),
    undefined,
  ),
)
for (const id in schema.types) {
  const ty = schema.types[id]
  if (ty.parent == null) {
    const discriminants = schema.serialization[id].discriminants
    if (discriminants == null) {
      emit(makeConcreteType(id, schema))
    } else {
      makeAbstractType(id, discriminants, schema)
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
