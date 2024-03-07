import { normalizeQualifiedName, qnFromSegments } from '@/util/qualifiedName'
import type {
  AstId,
  IdentifierOrOperatorIdentifier,
  MutableAst,
  NodeKey,
  Owned,
  QualifiedName,
  TokenId,
  TokenKey,
} from 'shared/ast'
import {
  Ast,
  BodyBlock,
  Function,
  Ident,
  MutableBodyBlock,
  MutableModule,
  OprApp,
  PropertyAccess,
  Token,
  isTokenId,
  print,
} from 'shared/ast'
export * from 'shared/ast'

export function deserialize(serialized: string): Owned {
  const parsed: SerializedPrintedSource = JSON.parse(serialized)
  // Not implemented: restoring serialized external IDs. This is not the best approach anyway;
  // Y.Js can't merge edits to objects when they're being serialized and deserialized.
  return Ast.parse(parsed.code)
}

interface SerializedInfoMap {
  nodes: Record<NodeKey, AstId[]>
  tokens: Record<TokenKey, TokenId>
}

interface SerializedPrintedSource {
  info: SerializedInfoMap
  code: string
}

export function serialize(ast: Ast): string {
  return JSON.stringify(print(ast))
}

export type TokenTree = (TokenTree | string)[]
export function tokenTree(root: Ast): TokenTree {
  const module = root.module
  return Array.from(root.concreteChildren(), (child) => {
    if (isTokenId(child.node)) {
      return module.getToken(child.node).code()
    } else {
      const node = module.tryGet(child.node)
      return node ? tokenTree(node) : '<missing>'
    }
  })
}

export function tokenTreeWithIds(root: Ast): TokenTree {
  const module = root.module
  return [
    root.externalId,
    ...Array.from(root.concreteChildren(), (child) => {
      if (isTokenId(child.node)) {
        return module.getToken(child.node).code()
      } else {
        const node = module.tryGet(child.node)
        return node ? tokenTreeWithIds(node) : ['<missing>']
      }
    }),
  ]
}

export function moduleMethodNames(topLevel: BodyBlock): Set<string> {
  const result = new Set<string>()
  for (const statement of topLevel.statements()) {
    const inner = statement.innerExpression()
    if (inner instanceof Function) {
      result.add(inner.name.code())
    }
  }
  return result
}

// FIXME: We should use alias analysis to handle ambiguous names correctly.
export function findModuleMethod(topLevel: BodyBlock, name: string): Function | undefined {
  for (const statement of topLevel.statements()) {
    const inner = statement.innerExpression()
    if (inner instanceof Function && inner.name.code() === name) {
      return inner
    }
  }
  return undefined
}

export function functionBlock(topLevel: BodyBlock, name: string) {
  const func = findModuleMethod(topLevel, name)
  if (!(func?.body instanceof BodyBlock)) return undefined
  return func.body
}

export function deleteFromParentBlock(ast: MutableAst) {
  const parent = ast.mutableParent()
  if (parent instanceof MutableBodyBlock)
    parent.updateLines((lines) => lines.filter((line) => line.expression?.node.id !== ast.id))
}

/** If the input is a chain of applications of the given left-associative operator, and all the leaves of the
 *  operator-application tree are identifier expressions, return the identifiers from left to right.
 *  This is analogous to `ast.code().split(operator)`, but type-enforcing.
 */
export function unrollOprChain(
  ast: Ast,
  leftAssociativeOperator: string,
): IdentifierOrOperatorIdentifier[] | null {
  const idents: IdentifierOrOperatorIdentifier[] = []
  let ast_: Ast | undefined = ast
  while (
    ast_ instanceof OprApp &&
    ast_.operator.ok &&
    ast_.operator.value.code() === leftAssociativeOperator
  ) {
    if (!(ast_.rhs instanceof Ident)) return null
    idents.unshift(ast_.rhs.code())
    ast_ = ast_.lhs
  }
  if (!(ast_ instanceof Ident)) return null
  idents.unshift(ast_.code())
  return idents
}

/** If the input is a chain of property accesses (uses of the `.` operator with a syntactic identifier on the RHS), and
 *  the value at the beginning of the sequence is an identifier expression, return all the identifiers from left to
 *  right. This is analogous to `ast.code().split('.')`, but type-enforcing.
 */
export function unrollPropertyAccess(ast: Ast): IdentifierOrOperatorIdentifier[] | null {
  const idents: IdentifierOrOperatorIdentifier[] = []
  let ast_: Ast | undefined = ast
  while (ast_ instanceof PropertyAccess) {
    idents.unshift(ast_.rhs.code())
    ast_ = ast_.lhs
  }
  if (!(ast_ instanceof Ident)) return null
  idents.unshift(ast_.code())
  return idents
}

export function parseIdent(ast: Ast): IdentifierOrOperatorIdentifier | null {
  if (ast instanceof Ident) {
    return ast.code()
  } else {
    return null
  }
}

export function parseIdents(ast: Ast): IdentifierOrOperatorIdentifier[] | null {
  return unrollOprChain(ast, ',')
}

export function parseQualifiedName(ast: Ast): QualifiedName | null {
  const idents = unrollPropertyAccess(ast)
  return idents && normalizeQualifiedName(qnFromSegments(idents))
}

/* Substitute `pattern` inside `expression` with `to`.
 * Replaces identifier, the whole qualified name, or the beginning of the qualified name (first segments of property access chain). */
export function substituteQualifiedName(
  module: MutableModule,
  expression: Ast,
  pattern: QualifiedName | IdentifierOrOperatorIdentifier,
  to: QualifiedName,
) {
  const expr = module.getVersion(expression) ?? expression
  if (expr instanceof PropertyAccess || expr instanceof Ident) {
    const qn = parseQualifiedName(expr)
    if (qn === pattern) {
      expr.updateValue(() => Ast.parse(to, module))
    } else if (qn && qn.startsWith(pattern)) {
      const withoutPattern = qn.replace(pattern, '')
      expr.updateValue(() => Ast.parse(to + withoutPattern, module))
    }
  } else {
    for (const child of expr.children()) {
      if (child instanceof Token) {
        continue
      }
      substituteQualifiedName(module, child, pattern, to)
    }
  }
}

declare const tokenKey: unique symbol
declare module '@/providers/widgetRegistry' {
  export interface WidgetInputTypes {
    [tokenKey]: Token
  }
}
