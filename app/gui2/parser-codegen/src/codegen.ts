import * as ts from "typescript"
import {SyntaxKind} from "typescript"
import * as fs from "fs"

type Schema = {
  types: [string: Schema.Type]
}
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
  export type Class = { class: "type"; id: string }
  export type Primitive = { class: "primitive"; type: PrimitiveType }
  export type Sequence = { class: "sequence"; type: TypeRef }
  export type Option = { class: "option"; type: TypeRef }
  export type Result = { class: "result"; type0: TypeRef, type1: TypeRef }
  export type PrimitiveType = "bool" | "u32" | "u64" | "i32" | "i64" | "char" | "string"
}

function fromSnake(ident: string, to: "camel" | "pascal", prefix?: string): string {
  const segments = []
  if (prefix !== undefined) {
    segments.push(...prefix.split("_"))
  }
  segments.push(...ident.split("_"))
  return segments.map((seg, i) => {
    if (to === "camel" && i === 0) {
      return seg
    } else {
      return seg.charAt(0).toUpperCase() + seg.slice(1)
    }
  }).join("")
}

function toPascal(ident: string, prefix?: string): string {
  return fromSnake(ident, "pascal", prefix)
}

function toCamel(ident: string, prefix?: string): string {
  return fromSnake(ident, "camel", prefix)
}

function legalizeIdent(ident: string): string {
  // FIXME: We should accept a renaming table as an input alongside the schema, then emit an error if a keyword is
  //  encountered ("constructor") or a field name is duplicated ("type").
  switch (ident) {
    case "constructor":
      return "ident"
    case "type":
      return "typeNode"
    default:
      return ident
  }
}

type ExpressionTransformer = ((expression: ts.Expression) => ts.Expression)

function primitiveReader(name: string): ExpressionTransformer {
  return cursor => ts.factory.createCallExpression(ts.factory.createPropertyAccessExpression(cursor, name), [], [])
}

function readerTransformer(name: string): (readElement: ExpressionTransformer) => ExpressionTransformer {
  const innerParameter = ts.factory.createIdentifier("element")
  return (readElement: ExpressionTransformer) => (cursor: ts.Expression) => {
    return ts.factory.createCallExpression(
      ts.factory.createPropertyAccessExpression(cursor, name),
      [],
      [
        ts.factory.createArrowFunction(
          [],
          [],
          [ts.factory.createParameterDeclaration([], undefined, innerParameter, undefined, cursorType, undefined)],
          undefined,
          undefined,
          readElement(innerParameter)
        )
      ]
    )
  }
}

function readerTransformer2(name: string): (readOk: ExpressionTransformer, readErr: ExpressionTransformer) => ExpressionTransformer {
  function makeArrow(reader: ExpressionTransformer, data: ts.Identifier) {
    return ts.factory.createArrowFunction(
      [],
      [],
      [ts.factory.createParameterDeclaration([], undefined, data, undefined, cursorType, undefined)],
      undefined,
      undefined,
      reader(data)
    )
  }
  const okData = ts.factory.createIdentifier("okData")
  const errData = ts.factory.createIdentifier("errData")
  return (readOk: ExpressionTransformer, readErr: ExpressionTransformer) => (cursor: ts.Expression) => {
    return ts.factory.createCallExpression(
      ts.factory.createPropertyAccessExpression(cursor, name),
      [],
      [makeArrow(readOk, okData), makeArrow(readErr, errData)]
    )
  }
}

function readerTransformerSized(name: string): (readElement: ExpressionTransformer, size: number) => ExpressionTransformer {
  const innerParameter = ts.factory.createIdentifier("element")
  return (readElement: ExpressionTransformer, size: number) => (cursor: ts.Expression) => {
    return ts.factory.createCallExpression(
      ts.factory.createPropertyAccessExpression(cursor, name),
      [],
      [
        ts.factory.createArrowFunction(
          [],
          [],
          [ts.factory.createParameterDeclaration([], undefined, innerParameter, undefined, cursorType, undefined)],
          undefined,
          undefined,
          readElement(innerParameter)
        ),
        ts.factory.createNumericLiteral(size)
      ]
    )
  }
}

function namespacedName(name: string, namespace?: string): string {
  if (namespace === undefined) {
    return toPascal(name)
  } else {
    return toPascal(namespace) + "." + toPascal(name)
  }
}

function abstractTypeReader(name: string): ExpressionTransformer {
  return (cursor: ts.Expression) =>
    ts.factory.createCallExpression(
      ts.factory.createPropertyAccessExpression(ts.factory.createIdentifier(toPascal(name)), "read"),
      [],
      [cursorMethods.readPointer(cursor)]
    )
}

function concreteTypeReader(name: string): ExpressionTransformer {
  return (cursor: ts.Expression) =>
    ts.factory.createCallExpression(
      ts.factory.createPropertyAccessExpression(ts.factory.createIdentifier(toPascal(name)), "read"),
      [],
      [cursor]
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

  static new(ref: Schema.TypeRef, types: [string: Schema.Type]): Type {
    const c = ref.class
    switch (c) {
      case "type":
        const ty = types[ref.id]
        const parent = types[ty.parent]
        const typeName = namespacedName(ty.name, parent?.name)
        const type = ts.factory.createTypeReferenceNode(typeName)
        if (Object.entries(ty.discriminants).length !== 0) {
          return new Type(type, abstractTypeReader(typeName), POINTER_SIZE)
        } else {
          return new Type(type, concreteTypeReader(typeName), ty.size)
        }
      case "primitive":
        const p = ref.type
        switch (p) {
          case "bool":
            return new Type(ts.factory.createTypeReferenceNode("boolean"), cursorMethods.readBool, 1)
          case "u32":
            return new Type(ts.factory.createTypeReferenceNode("number"), cursorMethods.readU32, 4)
          case "i32":
            return new Type(ts.factory.createTypeReferenceNode("number"), cursorMethods.readI32, 4)
          case "u64":
            return new Type(ts.factory.createTypeReferenceNode("number"), cursorMethods.readU64, 8)
          case "i64":
            return new Type(ts.factory.createTypeReferenceNode("number"), cursorMethods.readI64, 8)
          case "char":
            return new Type(ts.factory.createTypeReferenceNode("number"), cursorMethods.readU32, 4)
          case "string":
            return new Type(ts.factory.createTypeReferenceNode("string"), cursorMethods.readString, POINTER_SIZE)
          default:
            const _ = p satisfies never
            throw new Error("unreachable: PrimitiveType.type='" + p + "'")
        }
      case "sequence":
        return Type.sequence(Type.new(ref.type, types))
      case "option":
        return Type.option(Type.new(ref.type, types))
      case "result":
        return Type.result(Type.new(ref.type0, types), Type.new(ref.type1, types))
      default:
        const _ = c satisfies never
        throw new Error("unreachable: TypeRef.class='" + c + "' in " + JSON.stringify(ref))
    }
  }

  static sequence(element: Type): Type {
    return new Type(
      ts.factory.createTypeReferenceNode("Iterable", [element.type]),
      cursorMethods.readSequence(element.reader, element.size),
      POINTER_SIZE
    )
  }

  static option(element: Type): Type {
    return new Type(
      ts.factory.createUnionTypeNode([element.type, nullType]),
      cursorMethods.readOption(element.reader),
      POINTER_SIZE + 1
    )
  }

  static result(ok: Type, err: Type): Type {
    return new Type(
      ts.factory.createUnionTypeNode([ok.type, err.type]),
      cursorMethods.readResult(ok.reader, err.reader),
      POINTER_SIZE
    )
  }
}

function seekCursor(cursor: ts.Expression, offset: number): ts.Expression {
  if (offset === 0) {
    return cursor
  } else {
    return ts.factory.createCallExpression(
      ts.factory.createPropertyAccessExpression(cursor, "seek"),
      [],
      [ts.factory.createNumericLiteral(offset)]
    )
  }
}

function makeGetter(fieldName: string, fieldData: Schema.Field, types: [string: Schema.Type]): ts.GetAccessorDeclaration {
  const type = Type.new(fieldData.type, types)
  return ts.factory.createGetAccessorDeclaration(
    [],
    ts.factory.createIdentifier(legalizeIdent(toCamel(fieldName))),
    [],
    type.type,
    ts.factory.createBlock([
      ts.factory.createReturnStatement(
        type.reader(
          seekCursor(
            ts.factory.createPropertyAccessExpression(ts.factory.createThis(), cursorFieldIdent),
            fieldData.offset
          ),
        )
      )
    ])
  )
}

// Helper for a common case of constructing an assignment.
function createAssignmentStatement(left: ts.Expression, right: ts.Expression): ts.Statement {
  return ts.factory.createExpressionStatement(
    ts.factory.createBinaryExpression(
      left,
      SyntaxKind.EqualsToken,
      right,
    )
  )
}

function makeConcreteType(id: string, types: [string: Schema.Type]): ts.ClassDeclaration {
  const ident = ts.factory.createIdentifier(toPascal(types[id].name))
  const paramIdent = ts.factory.createIdentifier("cursor")
  const cursorParam = ts.factory.createParameterDeclaration([], undefined, paramIdent, undefined, cursorType, undefined)
  return makeClass(
    [modifiers.export],
    ident,
    [
      forwardToSuper(paramIdent, cursorType),
      ts.factory.createMethodDeclaration(
        [modifiers.static],
        undefined,
        "read",
        undefined,
        [],
        [cursorParam],
        ts.factory.createTypeReferenceNode(ident),
        ts.factory.createBlock([ts.factory.createReturnStatement(ts.factory.createNewExpression(ident, [], [paramIdent]))])
      )
    ],
    id,
    types
  )
}

function debugValue(ident: ts.Identifier): ts.Expression {
  const value = ts.factory.createPropertyAccessExpression(ts.factory.createThis(), ident)
  return ts.factory.createCallExpression(ts.factory.createIdentifier("debugHelper"), [], [value])
}

function makeDebugFunction(fields: [string, Schema.Field][]): ts.MethodDeclaration {
  const getterIdent = (fieldName: string) => ts.factory.createIdentifier(legalizeIdent(toCamel(fieldName)))
  return ts.factory.createMethodDeclaration(
    [],
    undefined,
    "debug",
    undefined,
    [],
    [],
    ts.factory.createTypeReferenceNode("any"),
    ts.factory.createBlock([
      ts.factory.createReturnStatement(
        ts.factory.createObjectLiteralExpression([
          ts.factory.createSpreadAssignment(
            ts.factory.createCallExpression(
              ts.factory.createPropertyAccessExpression(ts.factory.createSuper(), "debug"),
              [],
              []
            )
          ),
          ...fields.map(([name, field] : [string, Schema.Field]) =>
            ts.factory.createPropertyAssignment(getterIdent(name), debugValue(getterIdent(name)))
          )
        ])
      )]
    )
  )
}

function makeClass(modifiers: ts.Modifier[], name: ts.Identifier, members: ts.ClassElement[], id: string, types: [string: Schema.Type]): ts.ClassDeclaration {
  const ty = types[id]
  return ts.factory.createClassDeclaration(
    modifiers,
    name,
    undefined,
    [ts.factory.createHeritageClause(ts.SyntaxKind.ExtendsKeyword, [
      ts.factory.createExpressionWithTypeArguments(lazyObjectIdent, [])
    ])],
    [
      ...members,
      ...ty.fields.map(([name, field]: [string, Schema.Field]) => makeGetter(name, field, types)),
      makeDebugFunction(ty.fields)
    ],
  );
}

type ChildType = {
  definition: ts.ClassDeclaration
  reference: ts.TypeNode
  enumMember: ts.EnumMember
  case: ts.CaseClause
}

function makeChildType(parentName: string, base: ts.Identifier, id: string, discrim: string, types: [string: Schema.Type]): ChildType {
  const ty: Schema.Type = types[id]
  const name = toPascal(ty.name)
  const ident = ts.factory.createIdentifier(name)
  const cursorIdent = ts.factory.createIdentifier("cursor")
  const cursorParam = ts.factory.createParameterDeclaration([], undefined, cursorIdent, undefined, cursorType, undefined)
  const discrimInt = ts.factory.createNumericLiteral(parseInt(discrim, 10))
  return {
    definition: ts.factory.createClassDeclaration(
      [modifiers.export],
      name,
      undefined,
      [ts.factory.createHeritageClause(ts.SyntaxKind.ExtendsKeyword, [
        ts.factory.createExpressionWithTypeArguments(base, [])
      ])],
      [
        ts.factory.createPropertyDeclaration(
          [modifiers.readonly],
          "type",
          undefined,
          ts.factory.createTypeReferenceNode("Type." + name),
          undefined
        ),
        ts.factory.createConstructorDeclaration([], [
            ts.factory.createParameterDeclaration([], undefined, cursorIdent, undefined, cursorType, undefined)
          ],
          ts.factory.createBlock([
            ts.factory.createExpressionStatement(ts.factory.createCallExpression(
              ts.factory.createIdentifier("super"), [], [cursorIdent]
            )),
            createAssignmentStatement(
              ts.factory.createPropertyAccessExpression(ts.factory.createIdentifier("this"), "type"),
              ts.factory.createPropertyAccessExpression(ts.factory.createIdentifier("Type"), name)
            )
          ])
        ),
        ts.factory.createMethodDeclaration(
          [modifiers.static],
          undefined,
          "read",
          undefined,
          [],
          [cursorParam],
          ts.factory.createTypeReferenceNode(ident),
          ts.factory.createBlock([ts.factory.createReturnStatement(ts.factory.createNewExpression(ident, [], [cursorIdent]))])
        ),
        ...ty.fields.map(([name, field]: [string, Schema.Field]) => makeGetter(name, field, types)),
        makeDebugFunction(ty.fields)
      ]
    ),
    reference: ts.factory.createTypeReferenceNode(name),
    enumMember: ts.factory.createEnumMember(toPascal(types[id].name), discrimInt),
    case: ts.factory.createCaseClause(
      discrimInt,
      [ts.factory.createReturnStatement(
        ts.factory.createNewExpression(ident, [], [seekCursor(cursorIdent, 4)])
      )]
    )
  }
}

function forwardToSuper(ident: ts.Identifier, type: ts.TypeNode) {
  return ts.factory.createConstructorDeclaration([], [
      ts.factory.createParameterDeclaration([], undefined, ident, undefined, type, undefined)
    ],
    ts.factory.createBlock([
      ts.factory.createExpressionStatement(ts.factory.createCallExpression(
        ts.factory.createIdentifier("super"), [], [ident]
      ))
    ])
  )
}

function casesOrThrow(cases: ts.CaseClause[], error: string): ts.CaseBlock {
  return ts.factory.createCaseBlock(
    [
      ...cases,
      ts.factory.createDefaultClause([
        ts.factory.createThrowStatement(
          ts.factory.createNewExpression(ts.factory.createIdentifier("Error"), [], [ts.factory.createStringLiteral(error)])
        )
      ])
    ]
  )
}

function abstractTypeDeserializer(ident: ts.Identifier, cases: ts.CaseClause[]): ts.FunctionDeclaration {
  const cursorIdent = ts.factory.createIdentifier("cursor")
  return ts.factory.createFunctionDeclaration(
    [modifiers.export],
    undefined,
    "read",
    [],
    [ts.factory.createParameterDeclaration([], undefined, cursorIdent, undefined, cursorType, undefined)],
    ts.factory.createTypeReferenceNode(ident),
    ts.factory.createBlock([
      ts.factory.createSwitchStatement(
        cursorMethods.readU32(cursorIdent),
        casesOrThrow(cases, "Unexpected discriminant while deserializing.")
      )
    ])
  )
}

function makeAbstractType(id: string, types: [string: Schema.Type]) {
  const ty = types[id]
  const name = toPascal(ty.name)
  const ident = ts.factory.createIdentifier(name)
  const baseIdent = ts.factory.createIdentifier("AbstractBase")
  const childTypes = Array.from(Object.entries(ty.discriminants), ([discrim, id]: [string, string]) => makeChildType(name, baseIdent, id, discrim, types))
  const cursorIdent = ts.factory.createIdentifier("cursor")
  const moduleDecl =
    ts.factory.createModuleDeclaration(
      [modifiers.export],
      ident,
      ts.factory.createModuleBlock([
        makeClass([modifiers.abstract], baseIdent, [forwardToSuper(cursorIdent, cursorType)], id, types),
        ts.factory.createEnumDeclaration(
          [modifiers.export, modifiers.const],
          "Type",
          childTypes.map(child => child.enumMember)
        ),
        ...childTypes.map(child => child.definition),
        ts.factory.createTypeAliasDeclaration(
          [modifiers.export],
          ident,
          undefined,
          ts.factory.createUnionTypeNode(childTypes.map(child => child.reference))
        ),
        abstractTypeDeserializer(ident, childTypes.map(child => child.case))
      ])
    )
  const abstractTypeExport = ts.factory.createTypeAliasDeclaration(
    [modifiers.export],
    ident,
    undefined,
    ts.factory.createTypeReferenceNode(name + "." + name)
  )
  console.log(printer.printNode(ts.EmitHint.Unspecified, moduleDecl, file))
  console.log(printer.printNode(ts.EmitHint.Unspecified, abstractTypeExport, file))
}


// ============
// === Main ===
// ============

const file = ts.createSourceFile("source.ts", "", ts.ScriptTarget.ESNext, false, ts.ScriptKind.TS)
const printer = ts.createPrinter({newLine: ts.NewLineKind.LineFeed})
const lazyObjectIdent = ts.factory.createIdentifier("LazyObject")
const nullType = ts.factory.createTypeReferenceNode("null")
const cursorFieldIdent = ts.factory.createIdentifier("lazyObjectData")
const cursorType = ts.factory.createTypeReferenceNode("Cursor")
const modifiers = {
  export: ts.factory.createModifier(ts.SyntaxKind.ExportKeyword),
  const: ts.factory.createModifier(ts.SyntaxKind.ConstKeyword),
  readonly: ts.factory.createModifier(ts.SyntaxKind.ReadonlyKeyword),
  abstract: ts.factory.createModifier(ts.SyntaxKind.AbstractKeyword),
  static: ts.factory.createModifier(ts.SyntaxKind.StaticKeyword),
} as const
const cursorMethods = {
  readString: primitiveReader("readString"),
  readBool: primitiveReader("readBool"),
  readU32: primitiveReader("readU32"),
  readI32: primitiveReader("readI32"),
  readU64: primitiveReader("readU64"),
  readI64: primitiveReader("readI64"),
  readPointer: primitiveReader("readPointer"),
  readSequence: readerTransformerSized("readSequence"),
  readOption: readerTransformer("readOption"),
  readResult: readerTransformer2("readResult"),
} as const
const POINTER_SIZE: number = 4

const data: Schema = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'))
for (const id in data.types) {
  const ty = data.types[id]
  if (ty.parent === null) {
    if (Object.entries(data.types[id].discriminants).length === 0) {
      const decl = makeConcreteType(id, data.types)
      console.log(printer.printNode(ts.EmitHint.Unspecified, decl, file))
    } else {
      makeAbstractType(id, data.types)
    }
  } else {
    // Ignore child types; they are generated when `makeAbstractType` processes the parent.
  }
}
