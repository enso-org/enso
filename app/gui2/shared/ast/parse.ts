import * as map from 'lib0/map'
import {
  Token,
  asOwned,
  isTokenId,
  parentId,
  subtreeRoots,
  type AstId,
  type NodeChild,
  type Owned,
} from '.'
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
  const abstractor = new Abstractor(module, code)
  const root = abstractor.abstractTree(tree).node
  const spans = { tokens: abstractor.tokens, nodes: abstractor.nodes }
  return { root, spans, toRaw: abstractor.toRaw }
}

class Abstractor {
  private readonly module: MutableModule
  private readonly code: string
  readonly nodes: NodeSpanMap
  readonly tokens: TokenSpanMap
  readonly toRaw: Map<AstId, RawAst.Tree>

  constructor(module: MutableModule, code: string) {
    this.module = module
    this.code = code
    this.nodes = new Map()
    this.tokens = new Map()
    this.toRaw = new Map()
  }

  abstractTree(tree: RawAst.Tree): { whitespace: string | undefined; node: Owned } {
    const whitespaceStart = tree.whitespaceStartInCodeParsed
    const whitespaceEnd = whitespaceStart + tree.whitespaceLengthInCodeParsed
    const whitespace = this.code.substring(whitespaceStart, whitespaceEnd)
    const codeStart = whitespaceEnd
    const codeEnd = codeStart + tree.childrenLengthInCodeParsed
    const spanKey = nodeKey(codeStart, codeEnd - codeStart)
    let node: Owned
    switch (tree.type) {
      case RawAst.Tree.Type.BodyBlock: {
        const lines = Array.from(tree.statements, (line) => {
          const newline = this.abstractToken(line.newline)
          const expression = line.expression ? this.abstractTree(line.expression) : undefined
          return { newline, expression }
        })
        node = BodyBlock.concrete(this.module, lines)
        break
      }
      case RawAst.Tree.Type.Function: {
        const name = this.abstractTree(tree.name)
        const argumentDefinitions = Array.from(tree.args, (arg) => this.abstractChildren(arg))
        const equals = this.abstractToken(tree.equals)
        const body = tree.body !== undefined ? this.abstractTree(tree.body) : undefined
        node = Function.concrete(this.module, name, argumentDefinitions, equals, body)
        break
      }
      case RawAst.Tree.Type.Ident: {
        const token = this.abstractToken(tree.token)
        node = Ident.concrete(this.module, token)
        break
      }
      case RawAst.Tree.Type.Assignment: {
        const pattern = this.abstractTree(tree.pattern)
        const equals = this.abstractToken(tree.equals)
        const value = this.abstractTree(tree.expr)
        node = Assignment.concrete(this.module, pattern, equals, value)
        break
      }
      case RawAst.Tree.Type.App: {
        const func = this.abstractTree(tree.func)
        const arg = this.abstractTree(tree.arg)
        node = App.concrete(this.module, func, undefined, undefined, arg)
        break
      }
      case RawAst.Tree.Type.NamedApp: {
        const func = this.abstractTree(tree.func)
        const open = tree.open ? this.abstractToken(tree.open) : undefined
        const name = this.abstractToken(tree.name)
        const equals = this.abstractToken(tree.equals)
        const arg = this.abstractTree(tree.arg)
        const close = tree.close ? this.abstractToken(tree.close) : undefined
        const parens = open && close ? { open, close } : undefined
        const nameSpecification = { name, equals }
        node = App.concrete(this.module, func, parens, nameSpecification, arg)
        break
      }
      case RawAst.Tree.Type.UnaryOprApp: {
        const opr = this.abstractToken(tree.opr)
        const arg = tree.rhs ? this.abstractTree(tree.rhs) : undefined
        if (arg && opr.node.code() === '-') {
          node = NegationApp.concrete(this.module, opr, arg)
        } else {
          node = UnaryOprApp.concrete(this.module, opr, arg)
        }
        break
      }
      case RawAst.Tree.Type.OprApp: {
        const lhs = tree.lhs ? this.abstractTree(tree.lhs) : undefined
        const opr = tree.opr.ok
          ? [this.abstractToken(tree.opr.value)]
          : Array.from(tree.opr.error.payload.operators, this.abstractToken.bind(this))
        const rhs = tree.rhs ? this.abstractTree(tree.rhs) : undefined
        if (opr.length === 1 && opr[0]?.node.code() === '.' && rhs?.node instanceof MutableIdent) {
          // Propagate type.
          const rhs_ = { ...rhs, node: rhs.node }
          node = PropertyAccess.concrete(this.module, lhs, opr[0], rhs_)
        } else {
          node = OprApp.concrete(this.module, lhs, opr, rhs)
        }
        break
      }
      case RawAst.Tree.Type.Number: {
        const tokens = []
        if (tree.base) tokens.push(this.abstractToken(tree.base))
        if (tree.integer) tokens.push(this.abstractToken(tree.integer))
        if (tree.fractionalDigits) {
          tokens.push(this.abstractToken(tree.fractionalDigits.dot))
          tokens.push(this.abstractToken(tree.fractionalDigits.digits))
        }
        node = NumericLiteral.concrete(this.module, tokens)
        break
      }
      case RawAst.Tree.Type.Wildcard: {
        const token = this.abstractToken(tree.token)
        node = Wildcard.concrete(this.module, token)
        break
      }
      // These expression types are (or will be) used for backend analysis.
      // The frontend can ignore them, avoiding some problems with expressions sharing spans
      // (which makes it impossible to give them unique IDs in the current IdMap format).
      case RawAst.Tree.Type.OprSectionBoundary:
      case RawAst.Tree.Type.TemplateFunction:
        return { whitespace, node: this.abstractTree(tree.ast).node }
      case RawAst.Tree.Type.Invalid: {
        const expression = this.abstractTree(tree.ast)
        node = Invalid.concrete(this.module, expression)
        break
      }
      case RawAst.Tree.Type.Group: {
        const open = tree.open ? this.abstractToken(tree.open) : undefined
        const expression = tree.body ? this.abstractTree(tree.body) : undefined
        const close = tree.close ? this.abstractToken(tree.close) : undefined
        node = Group.concrete(this.module, open, expression, close)
        break
      }
      case RawAst.Tree.Type.TextLiteral: {
        const open = tree.open ? this.abstractToken(tree.open) : undefined
        const newline = tree.newline ? this.abstractToken(tree.newline) : undefined
        const elements = []
        for (const e of tree.elements) {
          elements.push(...this.abstractChildren(e))
        }
        const close = tree.close ? this.abstractToken(tree.close) : undefined
        node = TextLiteral.concrete(this.module, open, newline, elements, close)
        break
      }
      case RawAst.Tree.Type.Documented: {
        const open = this.abstractToken(tree.documentation.open)
        const elements = []
        for (const e of tree.documentation.elements) {
          elements.push(...this.abstractChildren(e))
        }
        const newlines = Array.from(tree.documentation.newlines, this.abstractToken.bind(this))
        const expression = tree.expression ? this.abstractTree(tree.expression) : undefined
        node = Documented.concrete(this.module, open, elements, newlines, expression)
        break
      }
      case RawAst.Tree.Type.Import: {
        const recurseBody = (tree: RawAst.Tree) => {
          const body = this.abstractTree(tree)
          if (body.node instanceof Invalid && body.node.code() === '') return undefined
          return body
        }
        const recurseSegment = (segment: RawAst.MultiSegmentAppSegment) => ({
          header: this.abstractToken(segment.header),
          body: segment.body ? recurseBody(segment.body) : undefined,
        })
        const polyglot = tree.polyglot ? recurseSegment(tree.polyglot) : undefined
        const from = tree.from ? recurseSegment(tree.from) : undefined
        const import_ = recurseSegment(tree.import)
        const all = tree.all ? this.abstractToken(tree.all) : undefined
        const as = tree.as ? recurseSegment(tree.as) : undefined
        const hiding = tree.hiding ? recurseSegment(tree.hiding) : undefined
        node = Import.concrete(this.module, polyglot, from, import_, all, as, hiding)
        break
      }
      default: {
        node = Generic.concrete(this.module, this.abstractChildren(tree))
      }
    }
    this.toRaw.set(node.id, tree)
    map.setIfUndefined(this.nodes, spanKey, (): Ast[] => []).unshift(node)
    return { node, whitespace }
  }

  private abstractToken(token: RawAst.Token): { whitespace: string; node: Token } {
    const whitespaceStart = token.whitespaceStartInCodeBuffer
    const whitespaceEnd = whitespaceStart + token.whitespaceLengthInCodeBuffer
    const whitespace = this.code.substring(whitespaceStart, whitespaceEnd)
    const codeStart = token.startInCodeBuffer
    const codeEnd = codeStart + token.lengthInCodeBuffer
    const tokenCode = this.code.substring(codeStart, codeEnd)
    const key = tokenKey(codeStart, codeEnd - codeStart)
    const node = Token.new(tokenCode, token.type)
    this.tokens.set(key, node)
    return { whitespace, node }
  }

  private abstractChildren(tree: LazyObject) {
    const children: NodeChild<Owned | Token>[] = []
    const visitor = (child: LazyObject) => {
      if (RawAst.Tree.isInstance(child)) {
        children.push(this.abstractTree(child))
      } else if (RawAst.Token.isInstance(child)) {
        children.push(this.abstractToken(child))
      } else {
        child.visitChildren(visitor)
      }
    }
    tree.visitChildren(visitor)
    return children
  }
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

/** @internal Used by `Ast.printSubtree`. Note that some AST types have overrides. */
export function printAst(
  ast: Ast,
  info: SpanMap,
  offset: number,
  parentIndent: string | undefined,
  verbatim?: boolean,
): string {
  let code = ''
  for (const child of ast.concreteChildren(verbatim)) {
    if (!isTokenId(child.node) && ast.module.checkedGet(child.node) === undefined) continue
    if (child.whitespace != null) {
      code += child.whitespace
    } else if (code.length != 0) {
      code += ' '
    }
    if (isTokenId(child.node)) {
      const tokenStart = offset + code.length
      const token = ast.module.getToken(child.node)
      const span = tokenKey(tokenStart, token.code().length)
      info.tokens.set(span, token)
      code += token.code()
    } else {
      const childNode = ast.module.checkedGet(child.node)
      assert(childNode != null)
      code += childNode.printSubtree(info, offset + code.length, parentIndent, verbatim)
      // Extra structural validation.
      assertEqual(childNode.id, child.node)
      if (parentId(childNode) !== ast.id) {
        console.error(`Inconsistent parent pointer (expected ${ast.id})`, childNode)
      }
      assertEqual(parentId(childNode), ast.id)
    }
  }
  const span = nodeKey(offset, code.length)
  map.setIfUndefined(info.nodes, span, (): Ast[] => []).unshift(ast)
  return code
}

/** @internal Use `Ast.code()' to stringify. */
export function printBlock(
  block: BodyBlock,
  info: SpanMap,
  offset: number,
  parentIndent: string | undefined,
  verbatim?: boolean,
): string {
  let blockIndent: string | undefined
  let code = ''
  for (const line of block.fields.get('lines')) {
    code += line.newline.whitespace ?? ''
    const newlineCode = block.module.getToken(line.newline.node).code()
    // Only print a newline if this isn't the first line in the output, or it's a comment.
    if (offset || code || newlineCode.startsWith('#')) {
      // If this isn't the first line in the output, but there is a concrete newline token:
      // if it's a zero-length newline, ignore it and print a normal newline.
      code += newlineCode || '\n'
    }
    if (line.expression) {
      if (blockIndent === undefined) {
        if ((line.expression.whitespace?.length ?? 0) > (parentIndent?.length ?? 0)) {
          blockIndent = line.expression.whitespace!
        } else if (parentIndent !== undefined) {
          blockIndent = parentIndent + '    '
        } else {
          blockIndent = ''
        }
      }
      const validIndent = (line.expression.whitespace?.length ?? 0) > (parentIndent?.length ?? 0)
      code += validIndent ? line.expression.whitespace : blockIndent
      const lineNode = block.module.checkedGet(line.expression.node)
      assertEqual(lineNode.id, line.expression.node)
      assertEqual(parentId(lineNode), block.id)
      code += lineNode.printSubtree(info, offset + code.length, blockIndent, verbatim)
    }
  }
  const span = nodeKey(offset, code.length)
  map.setIfUndefined(info.nodes, span, (): Ast[] => []).unshift(block)
  return code
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
