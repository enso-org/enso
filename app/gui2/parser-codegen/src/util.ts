import * as changeCase from 'change-case'

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
