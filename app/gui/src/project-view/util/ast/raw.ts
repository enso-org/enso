import * as RawAst from 'ydoc-shared/ast/generated/ast'
import { rawParseModule } from 'ydoc-shared/ast/parse'
import { LazyObject } from 'ydoc-shared/ast/parserSupport'
import { type SourceRange } from 'ydoc-shared/util/data/text'

export { RawAst, rawParseModule }

export type HasAstRange = SourceRange | RawAst.Tree | RawAst.Token

/**
 * Read span of code represented by given AST node, not including left whitespace offset.
 *
 * The AST is assumed to be generated from `code` and not modified sice then.
 * Otherwise an unspecified fragment of `code` may be returned.
 */
export function readAstOrTokenSpan(node: RawAst.Tree | RawAst.Token, code: string): string {
  const range = parsedTreeOrTokenRange(node)
  return code.substring(range.from, range.to)
}

/**
 * Read span of code represented by given RawAst.Token.
 *
 * The Token is assumed to be a part of AST generated from `code`.
 */
export function readTokenSpan(token: RawAst.Token, code: string): string {
  const range = parsedTokenRange(token)
  return code.substring(range.from, range.to)
}

/** TODO: Add docs */
export function childrenAstNodesOrTokens(obj: LazyObject): (RawAst.Tree | RawAst.Token)[] {
  const children: (RawAst.Tree | RawAst.Token)[] = []
  const visitor = (obj: LazyObject) => {
    if (RawAst.Tree.isInstance(obj) || RawAst.Token.isInstance(obj)) {
      children.push(obj)
    } else {
      obj.visitChildren(visitor)
    }
  }
  obj.visitChildren(visitor)
  return children
}

/** TODO: Add docs */
export function* walkRecursive(
  node: RawAst.Tree | RawAst.Token,
): Generator<RawAst.Tree | RawAst.Token, void, boolean | void> {
  if (false === (yield node)) return
  const stack: Iterator<RawAst.Tree | RawAst.Token>[] = [childrenAstNodesOrTokens(node).values()]
  while (stack.length > 0) {
    const next = stack[stack.length - 1]!.next()
    if (next.done) stack.pop()
    else if (false !== (yield next.value)) stack.push(childrenAstNodesOrTokens(next.value).values())
  }
}

/** TODO: Add docs */
export function visitGenerator<T, N, R>(generator: Generator<T, R, N>, visit: (value: T) => N): R {
  let next = generator.next()
  while (!next.done) next = generator.next(visit(next.value))
  return next.value
}

/**
 * Recursively visit AST nodes in depth-first order. The children of a node will be skipped when
 * `visit` callback returns `false`.
 * @param node Root node of the tree to walk. It will be visited first.
 * @param visit Callback that is called for each node. If it returns `false`, the children of that
 * node will be skipped, and the walk will continue to the next sibling.
 */
export function visitRecursive(
  node: RawAst.Tree | RawAst.Token,
  visit: (node: RawAst.Tree | RawAst.Token) => boolean,
) {
  visitGenerator(walkRecursive(node), visit)
}

/**
 * Read ast span information in `String.substring` compatible way. The returned span does not
 * include left whitespace offset.
 * @returns Object with `from` and `to` properties; index of first character in the `node`
 *   and first character _not_ being in the `node`.
 */
export function parsedTreeRange(tree: RawAst.Tree): SourceRange {
  const from = tree.whitespaceStartInCodeParsed + tree.whitespaceLengthInCodeParsed
  const to = from + tree.childrenLengthInCodeParsed
  return { from, to }
}

/** TODO: Add docs */
function parsedTokenRange(token: RawAst.Token): SourceRange {
  const from = token.startInCodeBuffer
  const to = from + token.lengthInCodeBuffer
  return { from, to }
}

/** TODO: Add docs */
export function parsedTreeOrTokenRange(node: HasAstRange): SourceRange {
  if (RawAst.Tree.isInstance(node)) return parsedTreeRange(node)
  else if (RawAst.Token.isInstance(node)) return parsedTokenRange(node)
  else return node
}
