/** Design:
 *
 * # "parse" Enso code to binary format.
 * # Visit nodes within blob.
 * # Visitation protocol requires no allocations; visitor can process the data and allocate only the final types.
 *   - All visitor arguments are integers within the SMI range.
 *
 */

/*
module Enso {
    function parse(text: string, id_map: Map<string, string>) {}
}

type Enso = Uint8Array
 */

type Tree = number

namespace Token {
    export type Any = number
    export type OpenSymbol = number
    export type CloseSymbol = number
    export type Ident = number
    export type Operator = number
}

interface Parsed {
    visitTree(tree: Tree, visitor: VisitTree): void;
    tokenCode(token: Token.Any): string;
    tokenWhitespace(token: Token.Any): number;
}

// Tree: Visitor implementation.
// Token: External-storage implementation.

interface VisitTree extends VisitNamedApp {}

interface VisitNamedApp {
    visitNamedApp(
        start: number,
        len: number,
        func: Tree,
        open: Token.OpenSymbol | undefined,
        name: Token.Ident,
        equals: Token.Operator,
        arg: Tree,
        close: Token.CloseSymbol | undefined
    ): void
}
