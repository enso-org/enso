import * as changeCase from 'change-case'
import ts from 'typescript'
const tsf = ts.factory

// === Identifier utilities ===

export function toPascal(ident: string): string {
  if (ident.includes('.')) throw new Error('toPascal cannot be applied to a namespaced name.')
  return changeCase.pascalCase(ident)
}

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

export function mapIdent(ident: string): string {
  return RENAME.get(ident) ?? ident
}

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

export function assignmentStatement(left: ts.Expression, right: ts.Expression): ts.Statement {
  return tsf.createExpressionStatement(
    tsf.createBinaryExpression(left, ts.SyntaxKind.EqualsToken, right),
  )
}

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

export function casesOrThrow(cases: ts.CaseClause[], error: string): ts.CaseBlock {
  return tsf.createCaseBlock([...cases, tsf.createDefaultClause([throwError(error)])])
}

export function throwError(error: string): ts.Statement {
  return tsf.createThrowStatement(
    tsf.createNewExpression(tsf.createIdentifier('Error'), [], [tsf.createStringLiteral(error)]),
  )
}

export function makeArrow(params: ts.BindingName[], expr: ts.Expression) {
  return tsf.createArrowFunction(
    [],
    [],
    params.map((ident) => tsf.createParameterDeclaration([], undefined, ident)),
    undefined,
    undefined,
    expr,
  )
}
