import { normalizeQualifiedName, qnFromSegments } from '@/util/qualifiedName'
import type {
  AstId,
  IdentifierOrOperatorIdentifier,
  Mutable,
  MutableAst,
  NodeKey,
  Owned,
  QualifiedName,
  TokenId,
  TokenKey,
} from 'ydoc-shared/ast'
import {
  Ast,
  BodyBlock,
  Function,
  Ident,
  MutableBodyBlock,
  MutableIdent,
  MutableModule,
  MutablePropertyAccess,
  NegationApp,
  NumericLiteral,
  OprApp,
  PropertyAccess,
  Token,
  isTokenId,
  print,
} from 'ydoc-shared/ast'
export * from 'ydoc-shared/ast'

/** TODO: Add docs */
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

/** TODO: Add docs */
export function serialize(ast: Ast): string {
  return JSON.stringify(print(ast))
}

export type TokenTree = (TokenTree | string)[]
/** TODO: Add docs */
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

/** TODO: Add docs */
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

/** TODO: Add docs */
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
/** TODO: Add docs */
export function findModuleMethod(topLevel: BodyBlock, name: string): Function | undefined {
  for (const statement of topLevel.statements()) {
    const inner = statement.innerExpression()
    if (inner instanceof Function && inner.name.code() === name) {
      return inner
    }
  }
  return undefined
}

/** TODO: Add docs */
export function functionBlock(topLevel: BodyBlock, name: string) {
  const func = findModuleMethod(topLevel, name)
  if (!(func?.body instanceof BodyBlock)) return undefined
  return func.body
}

/** TODO: Add docs */
export function deleteFromParentBlock(ast: MutableAst) {
  const parent = ast.mutableParent()
  if (parent instanceof MutableBodyBlock)
    parent.updateLines((lines) => lines.filter((line) => line.expression?.node.id !== ast.id))
}

/**
 * If the input is a chain of applications of the given left-associative operator, and all the leaves of the
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

/**
 * If the input is a chain of property accesses (uses of the `.` operator with a syntactic identifier on the RHS), and
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

/** TODO: Add docs */
export function parseIdent(ast: Ast): IdentifierOrOperatorIdentifier | null {
  if (ast instanceof Ident) {
    return ast.code()
  } else {
    return null
  }
}

/** TODO: Add docs */
export function parseIdents(ast: Ast): IdentifierOrOperatorIdentifier[] | null {
  return unrollOprChain(ast, ',')
}

/** TODO: Add docs */
export function parseQualifiedName(ast: Ast): QualifiedName | null {
  const idents = unrollPropertyAccess(ast)
  return idents && normalizeQualifiedName(qnFromSegments(idents))
}

/**
 * Substitute `pattern` inside `expression` with `to`.
 * Will only replace the first item in the property acccess chain.
 */
export function substituteIdentifier(
  expr: MutableAst,
  pattern: IdentifierOrOperatorIdentifier,
  to: IdentifierOrOperatorIdentifier,
) {
  if (expr instanceof MutableIdent && expr.code() === pattern) {
    expr.setToken(to)
  } else if (expr instanceof MutablePropertyAccess) {
    // Substitute only the first item in the property access chain.
    if (expr.lhs != null) substituteIdentifier(expr.lhs, pattern, to)
  } else {
    for (const child of expr.children()) {
      if (child instanceof Token) {
        continue
      }
      const mutableChild = expr.module.getVersion(child)
      substituteIdentifier(mutableChild, pattern, to)
    }
  }
}

/**
 * Substitute `pattern` inside `expression` with `to`.
 * Replaces identifier, the whole qualified name, or the beginning of the qualified name (first segments of property access chain).
 */
export function substituteQualifiedName(
  expr: MutableAst,
  pattern: QualifiedName | IdentifierOrOperatorIdentifier,
  to: QualifiedName,
) {
  if (expr instanceof MutablePropertyAccess || expr instanceof MutableIdent) {
    const qn = parseQualifiedName(expr)
    if (qn === pattern) {
      expr.updateValue(() => Ast.parse(to, expr.module))
    } else if (qn && qn.startsWith(pattern)) {
      const withoutPattern = qn.replace(pattern, '')
      expr.updateValue(() => Ast.parse(to + withoutPattern, expr.module))
    }
  } else {
    for (const child of expr.children()) {
      if (child instanceof Token) {
        continue
      }
      const mutableChild = expr.module.getVersion(child)
      substituteQualifiedName(mutableChild, pattern, to)
    }
  }
}

/**
 * Try to convert the number to an Enso value.
 *
 *  Returns `undefined` if the input is not a real number. NOTE: The current implementation doesn't support numbers that
 *  JS prints in scientific notation.
 */
export function tryNumberToEnso(value: number, module: MutableModule) {
  if (!Number.isFinite(value)) return
  const literal = NumericLiteral.tryParse(Math.abs(value).toString(), module)
  if (!literal)
    console.warn(`Not implemented: Converting scientific-notation number to Enso value`, value)
  if (literal && value < 0) {
    return NegationApp.new(module, literal)
  } else {
    return literal
  }
}

/** TODO: Add docs */
export function tryEnsoToNumber(ast: Ast) {
  const [sign, literal] = ast instanceof NegationApp ? [-1, ast.argument] : [1, ast]
  if (!(literal instanceof NumericLiteral)) return
  // JS parsing is accidentally the same as our rules for literals: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number#number_coercion
  // except `_` separators: https://stackoverflow.com/questions/72548282/why-does-number-constructor-fail-to-parse-numbers-with-separators
  return sign * Number(literal.code().replace(/_/g, ''))
}

/** TODO: Add docs */
export function copyIntoNewModule<T extends Ast>(ast: T): Owned<Mutable<T>> {
  const module = MutableModule.Transient()
  module.importCopy(ast)
  return module.getVersion(ast) as Owned<Mutable<T>>
}

declare const tokenKey: unique symbol
declare module '@/providers/widgetRegistry' {
  export interface WidgetInputTypes {
    [tokenKey]: Token
  }
}
