import * as changeCase from 'change-case'
import ts from "typescript";
const { factory: tsf } = ts


// === Identifier utilities ===

export function toPascal(ident: string): string {
    if (ident.includes('.')) throw new Error("toPascal cannot be applied to a namespaced name.")
    return changeCase.pascalCase(ident)
}

export function toCamel(ident: string): string {
    if (ident.includes('.')) throw new Error("toCamel cannot be applied to a namespaced name.")
    return changeCase.camelCase(ident)
}

export function legalizeIdent(ident: string): string {
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

export function forwardToSuper(ident: ts.Identifier, type: ts.TypeNode, modifiers?: ts.ModifierLike[]) {
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
