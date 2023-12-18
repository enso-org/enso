import * as RawAst from '@/generated/ast'
import { assert } from '@/util/assert'
import * as Ast from '@/util/ast/abstract'
import { AstExtended as RawAstExtended } from '@/util/ast/extended'
import { isResult, mapOk } from '@/util/data/result'
import { parse } from '@/util/ffi'
import { LazyObject, LazySequence } from '@/util/parserSupport'
import * as map from 'lib0/map'
import type { ContentRange } from 'shared/yjsModel'

export { Ast, RawAst, RawAstExtended }

export type HasAstRange = ContentRange | RawAst.Tree | RawAst.Token

export function parseEnso(code: string): RawAst.Tree {
  const blob = parse(code)
  return RawAst.Tree.read(new DataView(blob.buffer), blob.byteLength - 4)
}

/** Read a single line of code
 *
 * Is meant to be a helper for tests. If the code is multiline, an exception is raised.
 */
export function parseEnsoLine(code: string): RawAst.Tree {
  const block = parseEnso(code)
  assert(block.type === RawAst.Tree.Type.BodyBlock)
  const statements = block.statements[Symbol.iterator]()
  const firstLine = statements.next()
  assert(!firstLine.done)
  assert(!!statements.next().done)
  assert(firstLine.value.expression != null)
  return firstLine.value.expression
}

/**
 * Read span of code represented by given AST node, not including left whitespace offset.
 *
 * The AST is assumed to be generated from `code` and not modified sice then.
 * Otherwise an unspecified fragment of `code` may be returned.
 */
export function readAstOrTokenSpan(node: RawAst.Tree | RawAst.Token, code: string): string {
  const range = parsedTreeOrTokenRange(node)
  return code.substring(range[0], range[1])
}

/**
 * Read span of code represented by given RawAst.Tree.
 *
 * The Tree is assumed to be a part of AST generated from `code`.
 */
export function readAstSpan(node: RawAst.Tree, code: string): string {
  const range = parsedTreeRange(node)
  return code.substring(range[0], range[1])
}

/**
 * Read span of code represented by given RawAst.Token.
 *
 * The Token is assumed to be a part of AST generated from `code`.
 */
export function readTokenSpan(token: RawAst.Token, code: string): string {
  const range = parsedTokenRange(token)
  return code.substring(range[0], range[1])
}

/**
 * Read direct AST children.
 */
export function childrenAstNodes(obj: LazyObject): RawAst.Tree[] {
  const children: RawAst.Tree[] = []
  const visitor = (obj: LazyObject) => {
    if (RawAst.Tree.isInstance(obj)) children.push(obj)
    else if (!RawAst.Token.isInstance(obj)) obj.visitChildren(visitor)
  }
  obj.visitChildren(visitor)
  return children
}
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

/** Returns all AST nodes from `root` tree containing given char, starting from the most nested. */
export function astContainingChar(charIndex: number, root: RawAst.Tree): RawAst.Tree[] {
  return treePath(root, (node) => {
    const begin = node.whitespaceStartInCodeParsed + node.whitespaceLengthInCodeParsed
    const end = begin + node.childrenLengthInCodeParsed
    return charIndex >= begin && charIndex < end
  }).reverse()
}

/** Given a predicate, return a path from the root down the tree containing the
 *  first node at each level found to satisfy the predicate. */
function treePath(obj: LazyObject, pred: (node: RawAst.Tree) => boolean): RawAst.Tree[] {
  const path: RawAst.Tree[] = []
  const visitor = (obj: LazyObject) => {
    if (RawAst.Tree.isInstance(obj)) {
      const isMatch = pred(obj)
      if (isMatch) path.push(obj)
      return obj.visitChildren(visitor) || isMatch
    } else {
      return obj.visitChildren(visitor)
    }
  }
  obj.visitChildren(visitor)
  return path
}

export function findAstWithRange(
  root: RawAst.Tree | RawAst.Token,
  range: ContentRange,
): RawAst.Tree | RawAst.Token | undefined {
  for (const child of childrenAstNodes(root)) {
    const [begin, end] = parsedTreeOrTokenRange(child)
    if (begin === range[0] && end === range[1]) return child
    if (begin <= range[0] && end >= range[1]) return findAstWithRange(child, range)
  }
}

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

export function visitGenerator<T, N, R>(generator: Generator<T, R, N>, visit: (value: T) => N): R {
  let next = generator.next()
  while (!next.done) next = generator.next(visit(next.value))
  return next.value
}

/**
 * Recursively visit AST nodes in depth-first order. The children of a node will be skipped when
 * `visit` callback returns `false`.
 *
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
 *
 * @returns Object with `start` and `end` properties; index of first character in the `node`
 *   and first character _not_ being in the `node`.
 */
export function parsedTreeRange(tree: RawAst.Tree): ContentRange {
  const start = tree.whitespaceStartInCodeParsed + tree.whitespaceLengthInCodeParsed
  const end = start + tree.childrenLengthInCodeParsed
  return [start, end]
}

export function parsedTokenRange(token: RawAst.Token): ContentRange {
  const start = token.startInCodeBuffer
  const end = start + token.lengthInCodeBuffer
  return [start, end]
}

export function parsedTreeOrTokenRange(node: HasAstRange): ContentRange {
  if (RawAst.Tree.isInstance(node)) return parsedTreeRange(node)
  else if (RawAst.Token.isInstance(node)) return parsedTokenRange(node)
  else return node
}

export function astPrettyPrintType(obj: unknown): string | undefined {
  if (obj instanceof LazyObject && Object.hasOwnProperty.call(obj, 'type')) {
    const proto = Object.getPrototypeOf(obj)
    return proto?.constructor?.name
  }
}

export function debugAst(obj: unknown): unknown {
  if (obj instanceof LazyObject) {
    const fields = Object.fromEntries(
      allGetterNames(obj).map((k) => [k, debugAst((obj as any)[k])]),
    )
    if (Object.hasOwnProperty.call(obj, 'type')) {
      const className = astPrettyPrintType(obj)
      return { type: className, ...fields }
    } else {
      return fields
    }
  } else if (obj instanceof LazySequence) {
    return Array.from(obj, debugAst)
  } else if (isResult(obj)) {
    return mapOk(obj, debugAst)
  } else {
    return obj
  }
}

const protoGetters = new Map()
function allGetterNames(obj: object): string[] {
  let proto = Object.getPrototypeOf(obj)
  return map.setIfUndefined(protoGetters, proto, () => {
    const props = new Map<string, PropertyDescriptor>()
    do {
      for (const [name, prop] of Object.entries(Object.getOwnPropertyDescriptors(proto))) {
        if (!props.has(name)) props.set(name, prop)
      }
    } while ((proto = Object.getPrototypeOf(proto)))
    const getters = new Set<string>()
    for (const [name, prop] of props.entries()) {
      if (prop.get != null && prop.configurable && !debugHideFields.includes(name)) {
        getters.add(name)
      }
    }
    return [...getters]
  })
}

const debugHideFields = [
  '_v',
  '__proto__',
  'codeReprBegin',
  'codeReprLen',
  'leftOffsetCodeReprBegin',
  'leftOffsetCodeReprLen',
  'leftOffsetVisible',
  'spanLeftOffsetCodeReprBegin',
  'spanLeftOffsetCodeReprLen',
  'spanLeftOffsetVisible',
]
