import { qnFromSegments } from '@/util/qualifiedName'
import type {
  Expression,
  Identifier,
  IdentifierOrOperatorIdentifier,
  Mutable,
  MutableExpression,
  MutableStatement,
  Owned,
  QualifiedName,
  Statement,
} from 'ydoc-shared/ast'
import {
  App,
  Ast,
  BodyBlock,
  FunctionDef,
  Group,
  Ident,
  MutableAst,
  MutableBodyBlock,
  MutableFunctionDef,
  MutableIdent,
  MutableModule,
  MutablePropertyAccess,
  NegationApp,
  NumericLiteral,
  OprApp,
  PropertyAccess,
  Token,
  Wildcard,
  abstract,
  isTokenId,
  parseExpression,
  rawParseModule,
  setExternalIds,
} from 'ydoc-shared/ast'
import { spanMapToIdMap, spanMapToSpanGetter } from 'ydoc-shared/ast/idMap'
import { IdMap } from 'ydoc-shared/yjsModel'

export * from 'ydoc-shared/ast'

/** Given an output of {@link serializeExpression}, returns a deserialized expression. */
export function deserializeExpression(serialized: string): Owned<MutableExpression> {
  // Not implemented: restoring serialized external IDs. This is not the best approach anyway;
  // Y.Js can't merge edits to objects when they're being serialized and deserialized.
  return parseExpression(serialized)!
}

/** Returns a serialized representation of the expression. */
export function serializeExpression(ast: Expression): string {
  return ast.code()
}

export type TokenTree = (TokenTree | string)[]
/** Returns a debug representation. */
export function tokenTree(root: Ast): TokenTree {
  const module = root.module
  return Array.from(root.concreteChildren({ verbatim: false, indent: '' }), (child) => {
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
    ...Array.from(root.concreteChildren({ verbatim: false, indent: '' }), (child) => {
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
    if (statement instanceof FunctionDef) result.add(statement.name.code())
  }
  return result
}

export function findModuleMethod(
  topLevel: MutableBodyBlock,
  name: string,
): { statement: MutableFunctionDef; index: number } | undefined
export function findModuleMethod(
  topLevel: BodyBlock,
  name: string,
): { statement: FunctionDef; index: number } | undefined
/** Find the definition of the function with the specified name in the given block. */
export function findModuleMethod(
  topLevel: BodyBlock,
  name: string,
): { statement: FunctionDef; index: number } | undefined {
  // FIXME: We should use alias analysis to handle shadowing correctly.
  const isFunctionWithName = (statement: Statement, name: string) =>
    statement instanceof FunctionDef && statement.name.code() === name
  const index = topLevel.lines.findIndex(
    (line) => line.statement && isFunctionWithName(line.statement.node, name),
  )
  if (index === -1) return undefined
  const statement = topLevel.lines[index]!.statement!.node as FunctionDef
  return {
    /** The function definition. */
    statement,
    /** The index into the block's `lines` where the definition was found. */
    index,
  }
}

/** Delete the specified statement from its containing block. */
export function deleteFromParentBlock(ast: MutableStatement) {
  const parent = ast.mutableParent()
  if (parent instanceof MutableBodyBlock)
    parent.updateLines((lines) => lines.filter((line) => line.statement?.node.id !== ast.id))
}

/**
 * If the input is a chain of applications of the given left-associative operator, and all the leaves of the
 *  operator-application tree are identifier expressions, return the identifiers from left to right.
 *  This is analogous to `ast.code().split(operator)`, but type-enforcing.
 */
export function unrollOprChain(ast: Ast, leftAssociativeOperator: string): Identifier[] | null {
  const idents: Identifier[] = []
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
export function parseIdents(ast: Ast): Identifier[] | null {
  return unrollOprChain(ast, ',')
}

/** If the syntax tree represents a valid qualified name, return an equivalent {@link QualifiedName}. */
export function astToQualifiedName(ast: Ast): QualifiedName | null {
  const idents = unrollPropertyAccess(ast)
  return idents && qnFromSegments(idents)
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
    const qn = astToQualifiedName(expr)
    if (qn === pattern) {
      expr.updateValue(() => parseExpression(to, expr.module)!)
    } else if (qn && qn.startsWith(pattern)) {
      const withoutPattern = qn.replace(pattern, '')
      expr.updateValue(() => parseExpression(to + withoutPattern, expr.module)!)
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

/** Safely cast a mutable or owned value to its base type. */
export function dropMutability<T extends Ast>(value: Owned<Mutable<T>>): T {
  return value as unknown as T
}

function unwrapGroups(ast: Ast) {
  while (ast instanceof Group && ast.expression) ast = ast.expression
  return ast
}

/**
 * Tries to recognize inputs that are semantically-equivalent to a sequence of `App`s, and returns the arguments
 * identified and LHS of the analyzable chain.
 *
 * In particular, this function currently recognizes syntax used in visualization-preprocessor expressions.
 */
export function analyzeAppLike(ast: Ast): { func: Ast; args: Ast[] } {
  const deferredOperands = new Array<Ast>()
  while (
    ast instanceof OprApp &&
    ast.operator.ok &&
    ast.operator.value.code() === '<|' &&
    ast.lhs &&
    ast.rhs
  ) {
    deferredOperands.push(unwrapGroups(ast.rhs))
    ast = unwrapGroups(ast.lhs)
  }
  deferredOperands.reverse()
  const args = new Array<Ast>()
  while (ast instanceof App) {
    const deferredOperand = ast.argument instanceof Wildcard ? deferredOperands.pop() : undefined
    args.push(deferredOperand ?? unwrapGroups(ast.argument))
    ast = ast.function
  }
  args.reverse()
  return { func: ast, args }
}

/**
 * Unroll the provided chain of `PropertyAccess` nodes, returning the first non-access as `subject` and the accesses
 * from left-to-right.
 */
export function accessChain(ast: Expression): {
  subject: Expression
  accessChain: PropertyAccess[]
} {
  const accessChain = new Array<PropertyAccess>()
  while (ast instanceof PropertyAccess && ast.lhs) {
    accessChain.push(ast)
    ast = ast.lhs
  }
  accessChain.reverse()
  return { subject: ast, accessChain }
}

/**
 * Parse the input, and apply the given `IdMap`. Return the parsed tree, the updated `IdMap`, the span map, and a
 *  mapping to the `RawAst` representation.
 */
export function parseUpdatingIdMap(
  code: string,
  idMap?: IdMap | undefined,
  inModule?: MutableModule,
) {
  const rawRoot = rawParseModule(code)
  const module = inModule ?? MutableModule.Transient()
  const { root, spans } = module.transact(() => {
    const { root, spans } = abstract(module, rawRoot, code)
    root.module.setRoot(root)
    if (idMap) setExternalIds(root.module, spans, idMap)
    return { root, spans }
  })
  const getSpan = spanMapToSpanGetter(spans.nodes)
  const idMapOut = spanMapToIdMap(spans)
  return { root, idMap: idMapOut, getSpan }
}

declare const tokenKey: unique symbol
declare module '@/providers/widgetRegistry' {
  export interface WidgetInputTypes {
    [tokenKey]: Token
  }
}
