import * as changeCase from 'change-case'
import ts from 'typescript'
const tsf = ts.factory

// === Identifier utilities ===

/** Convert an identifier from an arbitrary case into PascalCase. */
export function toPascal(ident: string): string {
  if (ident.includes('.')) throw new Error('toPascal cannot be applied to a namespaced name.')
  return changeCase.pascalCase(ident)
}

/** Convert an identifier from an arbitrary case into camelCase. */
export function toCamel(ident: string): string {
  if (ident.includes('.')) throw new Error('toCamel cannot be applied to a namespaced name.')
  return changeCase.camelCase(ident)
}

const RENAME = new Map([
  // TS reserved words.
  ['constructor', 'ident'],
  ['type', 'typeNode'],
  // Rename source references to reflect our usage:
  // - In `Tree`s:
  ['spanLeftOffsetCodeStartUtf16', 'whitespaceStartInCodeParsed'],
  ['spanLeftOffsetCodeLenUtf16', 'whitespaceLengthInCodeParsed'],
  ['spanCodeLengthUtf16', 'childrenLengthInCodeParsed'],
  // - In `Tokens`s:
  ['leftOffsetCodeStartUtf16', 'whitespaceStartInCodeBuffer'],
  ['leftOffsetCodeLenUtf16', 'whitespaceLengthInCodeBuffer'],
  ['codeLenUtf16', 'lengthInCodeBuffer'],
  ['codeStartUtf16', 'startInCodeBuffer'],
])

/** Rename certain special-cased identifiers to avoid using language keywords, and for increased clarity. */
export function mapIdent(ident: string): string {
  return RENAME.get(ident) ?? ident
}

/** Return a name with an optional namespace, normalized to PascalCase. */
export function namespacedName(name: string, namespace?: string): string {
  if (namespace == null) {
    return toPascal(name)
  } else {
    return toPascal(namespace) + '.' + toPascal(name)
  }
}

// === AST utilities ===

export const modifiers = {
  export: tsf.createModifier(ts.SyntaxKind.ExportKeyword),
  const: tsf.createModifier(ts.SyntaxKind.ConstKeyword),
  readonly: tsf.createModifier(ts.SyntaxKind.ReadonlyKeyword),
  abstract: tsf.createModifier(ts.SyntaxKind.AbstractKeyword),
  static: tsf.createModifier(ts.SyntaxKind.StaticKeyword),
  protected: tsf.createModifier(ts.SyntaxKind.ProtectedKeyword),
} as const

/** Create a TypeScript assignment statement. */
export function assignmentStatement(left: ts.Expression, right: ts.Expression): ts.Statement {
  return tsf.createExpressionStatement(
    tsf.createBinaryExpression(left, ts.SyntaxKind.EqualsToken, right),
  )
}

/**
 * Create a TypeScript `class` constructor that forwards a single parameter to its parent class'
 * constructor.
 */
export function forwardToSuper(
  ident: ts.Identifier,
  type: ts.TypeNode,
  modifiers?: ts.ModifierLike[],
) {
  return tsf.createConstructorDeclaration(
    modifiers,
    [tsf.createParameterDeclaration([], undefined, ident, undefined, type, undefined)],
    tsf.createBlock([
      tsf.createExpressionStatement(
        tsf.createCallExpression(tsf.createIdentifier('super'), [], [ident]),
      ),
    ]),
  )
}

/**
 * Create a TypeScript `switch` statement with an additional `default` case that throws an error
 * with the given message.
 */
export function casesOrThrow(cases: ts.CaseClause[], error: string): ts.CaseBlock {
  return tsf.createCaseBlock([...cases, tsf.createDefaultClause([throwError(error)])])
}

/** Create a TypeScript `throw` statement. */
export function throwError(error: string): ts.Statement {
  return tsf.createThrowStatement(
    tsf.createNewExpression(tsf.createIdentifier('Error'), [], [tsf.createStringLiteral(error)]),
  )
}

/** Create a TypeScript `=>` function with the given single expression as its body. */
export function makeArrow(params: ts.BindingName[], expr: ts.Expression) {
  return tsf.createArrowFunction(
    [],
    [],
    params.map(ident => tsf.createParameterDeclaration([], undefined, ident)),
    undefined,
    undefined,
    expr,
  )
}
