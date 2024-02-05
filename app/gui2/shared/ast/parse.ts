import * as map from 'lib0/map'
import type { AstId, NodeChild, Owned } from '.'
import { Token, asOwned, parentId, subtreeRoots } from '.'
import { assert, assertDefined, assertEqual } from '../util/assert'
import type { SourceRange, SourceRangeKey } from '../yjsModel'
import { IdMap, isUuid, sourceRangeFromKey, sourceRangeKey } from '../yjsModel'
import { parse_tree } from './ffi'
import * as RawAst from './generated/ast'
import { MutableModule } from './mutableModule'
import type { LazyObject } from './parserSupport'
import {
  App,
  Assignment,
  Ast,
  BodyBlock,
  Documented,
  Function,
  Generic,
  Group,
  Ident,
  Import,
  Invalid,
  MutableBodyBlock,
  MutableIdent,
  NegationApp,
  NumericLiteral,
  OprApp,
  PropertyAccess,
  TextLiteral,
  UnaryOprApp,
  Wildcard,
} from './tree'

export function parseEnso(code: string): RawAst.Tree {
  const blob = parse_tree(code)
  return RawAst.Tree.read(new DataView(blob.buffer), blob.byteLength - 4)
}

export function normalize(rootIn: Ast): Ast {
  const printed = print(rootIn)
  const idMap = spanMapToIdMap(printed.info)
  const module = MutableModule.Transient()
  const tree = parseEnso(printed.code)
  const { root: parsed, spans } = abstract(module, tree, printed.code)
  module.replaceRoot(parsed)
  setExternalIds(module, spans, idMap)
  return parsed
}

export function abstract(
  module: MutableModule,
  tree: RawAst.Tree,
  code: string,
): { root: Owned; spans: SpanMap; toRaw: Map<AstId, RawAst.Tree> } {
  const tokens = new Map()
  const nodes = new Map()
  const toRaw = new Map()
  const root = abstractTree(module, tree, code, nodes, tokens, toRaw).node
  const spans = { tokens, nodes }
  return { root, spans, toRaw }
}

function abstractTree(
  module: MutableModule,
  tree: RawAst.Tree,
  code: string,
  nodesOut: NodeSpanMap,
  tokensOut: TokenSpanMap,
  toRaw: Map<AstId, RawAst.Tree>,
): { whitespace: string | undefined; node: Owned } {
  const recurseTree = (tree: RawAst.Tree) =>
    abstractTree(module, tree, code, nodesOut, tokensOut, toRaw)
  const recurseToken = (token: RawAst.Token.Token) => abstractToken(token, code, tokensOut)
  const visitChildren = (tree: LazyObject) => {
    const children: NodeChild<Owned | Token>[] = []
    const visitor = (child: LazyObject) => {
      if (RawAst.Tree.isInstance(child)) {
        children.push(recurseTree(child))
      } else if (RawAst.Token.isInstance(child)) {
        children.push(recurseToken(child))
      } else {
        child.visitChildren(visitor)
      }
    }
    tree.visitChildren(visitor)
    return children
  }
  const whitespaceStart = tree.whitespaceStartInCodeParsed
  const whitespaceEnd = whitespaceStart + tree.whitespaceLengthInCodeParsed
  const whitespace = code.substring(whitespaceStart, whitespaceEnd)
  const codeStart = whitespaceEnd
  const codeEnd = codeStart + tree.childrenLengthInCodeParsed
  const spanKey = nodeKey(codeStart, codeEnd - codeStart)
  let node: Owned
  switch (tree.type) {
    case RawAst.Tree.Type.BodyBlock: {
      const lines = Array.from(tree.statements, (line) => {
        const newline = recurseToken(line.newline)
        const expression = line.expression ? recurseTree(line.expression) : undefined
        return { newline, expression }
      })
      node = BodyBlock.concrete(module, lines)
      break
    }
    case RawAst.Tree.Type.Function: {
      const name = recurseTree(tree.name)
      const argumentDefinitions = Array.from(tree.args, (arg) => visitChildren(arg))
      const equals = recurseToken(tree.equals)
      const body = tree.body !== undefined ? recurseTree(tree.body) : undefined
      node = Function.concrete(module, name, argumentDefinitions, equals, body)
      break
    }
    case RawAst.Tree.Type.Ident: {
      const token = recurseToken(tree.token)
      node = Ident.concrete(module, token)
      break
    }
    case RawAst.Tree.Type.Assignment: {
      const pattern = recurseTree(tree.pattern)
      const equals = recurseToken(tree.equals)
      const value = recurseTree(tree.expr)
      node = Assignment.concrete(module, pattern, equals, value)
      break
    }
    case RawAst.Tree.Type.App: {
      const func = recurseTree(tree.func)
      const arg = recurseTree(tree.arg)
      node = App.concrete(module, func, undefined, undefined, arg)
      break
    }
    case RawAst.Tree.Type.NamedApp: {
      const func = recurseTree(tree.func)
      const open = tree.open ? recurseToken(tree.open) : undefined
      const name = recurseToken(tree.name)
      const equals = recurseToken(tree.equals)
      const arg = recurseTree(tree.arg)
      const close = tree.close ? recurseToken(tree.close) : undefined
      const parens = open && close ? { open, close } : undefined
      const nameSpecification = { name, equals }
      node = App.concrete(module, func, parens, nameSpecification, arg)
      break
    }
    case RawAst.Tree.Type.UnaryOprApp: {
      const opr = recurseToken(tree.opr)
      const arg = tree.rhs ? recurseTree(tree.rhs) : undefined
      if (arg && opr.node.code() === '-') {
        node = NegationApp.concrete(module, opr, arg)
      } else {
        node = UnaryOprApp.concrete(module, opr, arg)
      }
      break
    }
    case RawAst.Tree.Type.OprApp: {
      const lhs = tree.lhs ? recurseTree(tree.lhs) : undefined
      const opr = tree.opr.ok
        ? [recurseToken(tree.opr.value)]
        : Array.from(tree.opr.error.payload.operators, recurseToken)
      const rhs = tree.rhs ? recurseTree(tree.rhs) : undefined
      if (opr.length === 1 && opr[0]?.node.code() === '.' && rhs?.node instanceof MutableIdent) {
        // Propagate type.
        const rhs_ = { ...rhs, node: rhs.node }
        node = PropertyAccess.concrete(module, lhs, opr[0], rhs_)
      } else {
        node = OprApp.concrete(module, lhs, opr, rhs)
      }
      break
    }
    case RawAst.Tree.Type.Number: {
      const tokens = []
      if (tree.base) tokens.push(recurseToken(tree.base))
      if (tree.integer) tokens.push(recurseToken(tree.integer))
      if (tree.fractionalDigits) {
        tokens.push(recurseToken(tree.fractionalDigits.dot))
        tokens.push(recurseToken(tree.fractionalDigits.digits))
      }
      node = NumericLiteral.concrete(module, tokens)
      break
    }
    case RawAst.Tree.Type.Wildcard: {
      const token = recurseToken(tree.token)
      node = Wildcard.concrete(module, token)
      break
    }
    // These expression types are (or will be) used for backend analysis.
    // The frontend can ignore them, avoiding some problems with expressions sharing spans
    // (which makes it impossible to give them unique IDs in the current IdMap format).
    case RawAst.Tree.Type.OprSectionBoundary:
    case RawAst.Tree.Type.TemplateFunction:
      return { whitespace, node: recurseTree(tree.ast).node }
    case RawAst.Tree.Type.Invalid: {
      const expression = recurseTree(tree.ast)
      node = Invalid.concrete(module, expression)
      break
    }
    case RawAst.Tree.Type.Group: {
      const open = tree.open ? recurseToken(tree.open) : undefined
      const expression = tree.body ? recurseTree(tree.body) : undefined
      const close = tree.close ? recurseToken(tree.close) : undefined
      node = Group.concrete(module, open, expression, close)
      break
    }
    case RawAst.Tree.Type.TextLiteral: {
      const open = tree.open ? recurseToken(tree.open) : undefined
      const newline = tree.newline ? recurseToken(tree.newline) : undefined
      const elements = []
      for (const e of tree.elements) {
        elements.push(...visitChildren(e))
      }
      const close = tree.close ? recurseToken(tree.close) : undefined
      node = TextLiteral.concrete(module, open, newline, elements, close)
      break
    }
    case RawAst.Tree.Type.Documented: {
      const open = recurseToken(tree.documentation.open)
      const elements = []
      for (const e of tree.documentation.elements) {
        elements.push(...visitChildren(e))
      }
      const newlines = Array.from(tree.documentation.newlines, recurseToken)
      const expression = tree.expression ? recurseTree(tree.expression) : undefined
      node = Documented.concrete(module, open, elements, newlines, expression)
      break
    }
    case RawAst.Tree.Type.Import: {
      const recurseBody = (tree: RawAst.Tree) => {
        const body = recurseTree(tree)
        if (body.node instanceof Invalid && body.node.code() === '') return undefined
        return body
      }
      const recurseSegment = (segment: RawAst.MultiSegmentAppSegment) => ({
        header: recurseToken(segment.header),
        body: segment.body ? recurseBody(segment.body) : undefined,
      })
      const polyglot = tree.polyglot ? recurseSegment(tree.polyglot) : undefined
      const from = tree.from ? recurseSegment(tree.from) : undefined
      const import_ = recurseSegment(tree.import)
      const all = tree.all ? recurseToken(tree.all) : undefined
      const as = tree.as ? recurseSegment(tree.as) : undefined
      const hiding = tree.hiding ? recurseSegment(tree.hiding) : undefined
      node = Import.concrete(module, polyglot, from, import_, all, as, hiding)
      break
    }
    default: {
      node = Generic.concrete(module, visitChildren(tree))
    }
  }
  toRaw.set(node.id, tree)
  map.setIfUndefined(nodesOut, spanKey, (): Ast[] => []).unshift(node)
  return { node, whitespace }
}

function abstractToken(
  token: RawAst.Token,
  code: string,
  tokensOut: TokenSpanMap,
): { whitespace: string; node: Token } {
  const whitespaceStart = token.whitespaceStartInCodeBuffer
  const whitespaceEnd = whitespaceStart + token.whitespaceLengthInCodeBuffer
  const whitespace = code.substring(whitespaceStart, whitespaceEnd)
  const codeStart = token.startInCodeBuffer
  const codeEnd = codeStart + token.lengthInCodeBuffer
  const tokenCode = code.substring(codeStart, codeEnd)
  const key = tokenKey(codeStart, codeEnd - codeStart)
  const node = Token.new(tokenCode, token.type)
  tokensOut.set(key, node)
  return { whitespace, node }
}

declare const nodeKeyBrand: unique symbol
export type NodeKey = SourceRangeKey & { [nodeKeyBrand]: never }
declare const tokenKeyBrand: unique symbol
export type TokenKey = SourceRangeKey & { [tokenKeyBrand]: never }
export function nodeKey(start: number, length: number): NodeKey {
  return sourceRangeKey([start, start + length]) as NodeKey
}
export function tokenKey(start: number, length: number): TokenKey {
  return sourceRangeKey([start, start + length]) as TokenKey
}

export type NodeSpanMap = Map<NodeKey, Ast[]>
export type TokenSpanMap = Map<TokenKey, Token>

export interface SpanMap {
  nodes: NodeSpanMap
  tokens: TokenSpanMap
}

interface PrintedSource {
  info: SpanMap
  code: string
}

export function spanMapToIdMap(spans: SpanMap): IdMap {
  const idMap = new IdMap()
  for (const [key, token] of spans.tokens.entries()) {
    assert(isUuid(token.id))
    idMap.insertKnownId(sourceRangeFromKey(key), token.id)
  }
  for (const [key, asts] of spans.nodes.entries()) {
    for (const ast of asts) {
      assert(isUuid(ast.externalId))
      idMap.insertKnownId(sourceRangeFromKey(key), ast.externalId)
    }
  }
  return idMap
}

export function spanMapToSpanGetter(spans: SpanMap): (id: AstId) => SourceRange | undefined {
  const reverseMap = new Map<AstId, SourceRange>()
  for (const [key, asts] of spans.nodes) {
    for (const ast of asts) {
      reverseMap.set(ast.id, sourceRangeFromKey(key))
    }
  }
  return (id) => reverseMap.get(id)
}

/** Return stringification with associated ID map. This is only exported for testing. */
export function print(ast: Ast): PrintedSource {
  const info: SpanMap = {
    nodes: new Map(),
    tokens: new Map(),
  }
  const code = ast.printSubtree(info, 0, undefined)
  return { info, code }
}

/** Parse the input as a block. */
export function parseBlock(code: string, inModule?: MutableModule) {
  return parseBlockWithSpans(code, inModule).root
}

/** Parse the input. If it contains a single expression at the top level, return it; otherwise, return a block. */
export function parse(code: string, module?: MutableModule): Owned {
  const module_ = module ?? MutableModule.Transient()
  const ast = parseBlock(code, module_)
  const [expr] = ast.statements()
  if (!expr) return ast
  const parent = parentId(expr)
  if (parent) module_.delete(parent)
  expr.fields.set('parent', undefined)
  return asOwned(expr)
}

export function parseBlockWithSpans(
  code: string,
  inModule?: MutableModule,
): { root: Owned<MutableBodyBlock>; spans: SpanMap } {
  const tree = parseEnso(code)
  const module = inModule ?? MutableModule.Transient()
  return fromRaw(tree, code, module)
}

function fromRaw(
  tree: RawAst.Tree,
  code: string,
  inModule?: MutableModule,
): {
  root: Owned<MutableBodyBlock>
  spans: SpanMap
  toRaw: Map<AstId, RawAst.Tree>
} {
  const module = inModule ?? MutableModule.Transient()
  const ast = abstract(module, tree, code)
  const spans = ast.spans
  // The root of the tree produced by the parser is always a `BodyBlock`.
  const root = ast.root as Owned<MutableBodyBlock>
  return { root, spans, toRaw: ast.toRaw }
}

export function parseExtended(code: string, idMap?: IdMap | undefined, inModule?: MutableModule) {
  const rawRoot = parseEnso(code)
  const module = inModule ?? MutableModule.Transient()
  const { root, spans, toRaw, idMapUpdates } = module.ydoc.transact(() => {
    const { root, spans, toRaw } = fromRaw(rawRoot, code, module)
    root.module.replaceRoot(root)
    const idMapUpdates = idMap ? setExternalIds(root.module, spans, idMap) : 0
    return { root, spans, toRaw, idMapUpdates }
  }, 'local')
  const getSpan = spanMapToSpanGetter(spans)
  const idMapOut = spanMapToIdMap(spans)
  return { root, idMap: idMapOut, getSpan, toRaw, idMapUpdates }
}

export function setExternalIds(edit: MutableModule, spans: SpanMap, ids: IdMap) {
  let astsMatched = 0
  let idsUnmatched = 0
  let asts = 0
  edit.root()?.visitRecursiveAst((_ast) => (asts += 1))
  for (const [key, externalId] of ids.entries()) {
    const asts = spans.nodes.get(key as NodeKey)
    if (asts) {
      for (const ast of asts) {
        astsMatched += 1
        const editAst = edit.getVersion(ast)
        if (editAst.externalId !== externalId) editAst.setExternalId(externalId)
      }
    } else {
      idsUnmatched += 1
    }
  }
  return edit.root() ? asts - astsMatched : 0
}

function checkSpans(expected: NodeSpanMap, encountered: NodeSpanMap, code: string) {
  const lost = new Array<readonly [NodeKey, Ast]>()
  for (const [key, asts] of expected) {
    const outermostPrinted = asts[0]
    if (!outermostPrinted) continue
    for (let i = 1; i < asts.length; ++i) assertEqual(asts[i]?.parentId, asts[i - 1]?.id)
    const encounteredAsts = encountered.get(key)
    if (encounteredAsts === undefined) lost.push([key, outermostPrinted])
  }
  const lostInline = new Array<Ast>()
  const lostBlock = new Array<Ast>()
  for (const [key, ast] of lost) {
    const [start, end] = sourceRangeFromKey(key)
    ;(code.substring(start, end).match(/[\r\n]/) ? lostBlock : lostInline).push(ast)
  }
  return { lostInline, lostBlock }
}

/** If the input tree's concrete syntax has precedence errors (i.e. its expected code would not parse back to the same
 *  structure), try to fix it. If possible, it will be repaired by inserting parentheses; if that doesn't fix it, the
 *  affected subtree will be re-synced to faithfully represent the source code the incorrect tree prints to.
 */
export function repair(
  root: BodyBlock,
  module?: MutableModule,
): { code: string; fixes: MutableModule | undefined } {
  // Print the input to see what spans its nodes expect to have in the output.
  const printed = print(root)
  // Parse the printed output to see what spans actually correspond to nodes in the printed code.
  const reparsed = parseBlockWithSpans(printed.code)
  // See if any span we expected to be a node isn't; if so, it likely merged with its parent due to wrong precedence.
  const { lostInline, lostBlock } = checkSpans(
    printed.info.nodes,
    reparsed.spans.nodes,
    printed.code,
  )
  if (lostInline.length === 0) {
    if (lostBlock.length !== 0) {
      console.warn(`repair: Bad block elements, but all inline elements OK?`)
      const fixes = module ?? root.module.edit()
      resync(lostBlock, printed.info.nodes, reparsed.spans.nodes, fixes)
      return { code: printed.code, fixes }
    }
    return { code: printed.code, fixes: undefined }
  }

  // Wrap any "lost" nodes in parentheses.
  const fixes = module ?? root.module.edit()
  for (const ast of lostInline) {
    if (ast instanceof Group) continue
    fixes.getVersion(ast).update((ast) => Group.new(fixes, ast))
  }

  // Verify that it's fixed.
  const printed2 = print(fixes.getVersion(root))
  const reparsed2 = parseBlockWithSpans(printed2.code)
  const { lostInline: lostInline2, lostBlock: lostBlock2 } = checkSpans(
    printed2.info.nodes,
    reparsed2.spans.nodes,
    printed2.code,
  )
  if (lostInline2.length !== 0 || lostBlock2.length !== 0)
    resync([...lostInline2, ...lostBlock2], printed2.info.nodes, reparsed2.spans.nodes, fixes)

  return { code: printed2.code, fixes }
}

/**
 * Replace subtrees in the module to ensure that the module contents are consistent with the module's code.
 *
 * @param badAsts - ASTs that, if printed, would not parse to exactly their current content.
 * @param badSpans - Span map produced by printing the `badAsts` nodes and all their parents.
 * @param goodSpans - Span map produced by parsing the code from the module of `badAsts`.
 * @param edit - Module to apply the fixes to; must contain all ASTs in `badAsts`.
 */
function resync(
  badAsts: Iterable<Ast>,
  badSpans: NodeSpanMap,
  goodSpans: NodeSpanMap,
  edit: MutableModule,
) {
  const parentsOfBadSubtrees = new Set<AstId>()
  const badAstIds = new Set(Array.from(badAsts, (ast) => ast.id))
  for (const id of subtreeRoots(edit, badAstIds)) {
    const parent = edit.checkedGet(id)?.parentId
    if (parent) parentsOfBadSubtrees.add(parent)
  }

  const spanOfBadParent = new Array<readonly [AstId, NodeKey]>()
  for (const [span, asts] of badSpans) {
    for (const ast of asts) {
      if (parentsOfBadSubtrees.has(ast.id)) spanOfBadParent.push([ast.id, span])
    }
  }
  // All ASTs in the module of badAsts should have entries in badSpans.
  assertEqual(spanOfBadParent.length, parentsOfBadSubtrees.size)

  for (const [id, span] of spanOfBadParent) {
    const parent = edit.checkedGet(id)
    const goodAst = goodSpans.get(span)?.[0]
    // The parent of the root of a bad subtree must be a good AST.
    assertDefined(goodAst)
    parent.replaceValue(edit.copy(goodAst))
  }

  console.warn(
    `repair: Replaced ${parentsOfBadSubtrees.size} subtrees with their reparsed equivalents.`,
    parentsOfBadSubtrees,
  )
}
