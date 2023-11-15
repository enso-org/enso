/**
 * # AST
 *
 * The goal of the new AST is to make expression edits simple.
 *
 * ## Nodes
 *
 * A node has an ExprId and other metadata, type-specific information, and a sequence of children. Children can be node
 * references or strings. String children are the tokens of the frontend AST. For example, an identifier node has a
 * single string child; a parser "macro" contains strings for its reserved words and and expression references for its
 * general expressions.
 *
 * Only nodes have ExprId assignments and thus are able to receive type info from the backend. Some things that are
 * only tokens in `enso-parser` have nodes in the expression tree, e.g. operator symbols.
 *
 * ### Referential transparency
 *
 * Nodes are not dependent on any auxiliary data structure. This allows them to be easily created and rearranged.
 *
 * ## The AST is abstract
 *
 * `Tree` is concrete; this is important for preserving information such as exact whitespace characters, but it is not
 * conducive to editing. The GUI's AST is abstract. Avoiding redundant information allows application logic to perform
 * edits without needing to maintain unnecessary invariants. When ASTs are printed to text, any tokens needed to
 * represent the structure are materialized (e.g. parentheses are inserted as needed.) During this printing process,
 * the `Tree`s corresponding to preexisting AST nodes are consulted, in order to ensure that the concrete syntax of
 * unmodified parts of the output matches the input. This is the only use of `Tree` in the frontend outside of the
 * process of translating from `Tree` to frontend AST.
 *
 * ## AST graph
 *
 * Nodes are stored in a map indexed by internal IDs.
 *
 * The internal IDs are distinct from ExprIds because their consistency is driven by different concerns: E.g. when the
 * frontend is modifying a value atomically, it does so by replacing a child expression [more on this in the
 * synchronization documentation, to be written]; however the backend can reevaluate more efficiently if this operation
 * doesn't change the ExprId, so the new node should have the same ExprID as the node it replaces.
 *
 * ## Executing edits
 *
 * ### Dirty-node overlay
 *
 * There are two maps holding nodes: A primary map contains committed nodes. A dirty-node overlay map contains new or
 * modified nodes. Application logic performs edits by inserting (or modifying) entries in the dirty-node map. When
 * resolving a node reference, the overlay map is always checked first.
 *
 * Dirty nodes are ephemeral; they can only be committed in the same frame that they are created. This avoids a
 * tree-edit becoming outdated by remote changes; all conflict resolution is the responsibility of the Yjs CDRT.
 *
 * ### Print-parse roundtripping
 *
 * It is important that the AST is always consistent with the AST that would result from parsing the corresponding
 * source code. To ensure this, after executing the changes of an edit but before committing them, the modified
 * subtree(s) of the AST are printed and reparsed.
 *
 * AST-printing produces two outputs: Source code, and an ID map attaching identities to spans. This map is read when
 * translating the parsed `Tree`s to AST nodes, and used to reattach IDs. In addition to spans it stores node-type
 * information, which is used to disambiguate nodes that contain the same code (i.e. when a node contains one other
 * node as a child and has no other tokens or whitespace). Update operations on the map are not needed; the map is
 * written once while printing, and read once while translating.
 *
 * ### Updating the committed-node map
 *
 * The primary map is only updated in response to Yjs events. When an edit is completed, all the nodes in the overlay
 * map are used to produce modifications to the Yjs data structures, and the overlay is cleared. By using the Yjs
 * notifications even for local changes, we avoid needing separate tree-update mechanisms for local and remote changes.
 *
 * [In the initial implementation, until the new AST is used for synchronization, local edits can be committed by
 * simply moving dirty nodes into the committed node map. Remote edits can recreate the committed-node map by parsing
 * the new code.]
 */

import { Token, Tree } from '@/generated/ast'
import { parseEnso } from '@/util/ast'
import type { LazyObject } from '@/util/parserSupport'

declare const brandAstId: unique symbol
export type AstId = number & { [brandAstId]: never }

declare const brandTokenId: unique symbol
export type TokenId = number & { [brandTokenId]: never }

let nextNodeId = 0
function newNodeId(): AstId {
  const id = nextNodeId
  nextNodeId++
  return id as AstId
}

let nextTokenId = 0
function newTokenId(): TokenId {
  const id = nextTokenId
  nextTokenId++
  return id as TokenId
}

export type Tok = { code: string; id?: TokenId }
export type NodeChild = { whitespace?: string | undefined; node: AstId | Tok }

function isToken(node: Tok | AstId): node is Tok {
  return typeof node == 'object'
}

export class AbstractNode {
  protected _children: NodeChild[]
  public treeType: Tree.Type | undefined

  constructor(children?: NodeChild[], treeType?: Tree.Type) {
    this._children = children ?? []
    this.treeType = treeType
  }

  get children(): NodeChild[] {
    return this._children
  }
}
type FunctionArgument = NodeChild[]
type TokWithWhitespace = { whitespace?: string; node: Tok }
export class Function extends AbstractNode {
  public name: NodeChild
  public args: FunctionArgument[]
  public equals: TokWithWhitespace | undefined
  public body: NodeChild | null
  constructor(
    name: NodeChild,
    args: FunctionArgument[],
    equals: TokWithWhitespace | undefined,
    body: NodeChild | null,
  ) {
    super([], Tree.Type.Function)
    this.name = name
    this.args = args
    this.equals = equals
    this.body = body
  }
  static new(
    nodes: NodeMap,
    name: NodeChild,
    args: FunctionArgument[],
    equals: TokWithWhitespace | undefined,
    body: NodeChild | null,
  ): Function {
    return new Function(name, args, equals, body)
  }
  get children(): NodeChild[] {
    const children = [this.name]
    for (const arg of this.args)
        children.push(...arg)
    if (this.equals !== undefined) {
      children.push({ whitespace: this.equals.whitespace ?? ' ', node: this.equals.node })
    } else {
      children.push({ whitespace: ' ', node: { code: '=' } })
    }
    if (this.body !== null) {
      children.push(this.body)
    }
    return children
  }
}
type BlockLine = {
  newline?: TokWithWhitespace
  expression: NodeChild | null
}
export class Block extends AbstractNode {
  public lines: BlockLine[]

  constructor(lines: BlockLine[]) {
    super([], Tree.Type.BodyBlock)
    this.lines = lines
  }

  get children(): NodeChild[] {
    const children: NodeChild[] = []
    for (const line of this.lines) {
      if (line.newline !== undefined) {
        children.push(line.newline)
      } else {
        children.push({ node: { code: '\n' } })
      }
      if (line.expression !== null) children.push(line.expression)
    }
    return children
  }
}
export class Assignment extends AbstractNode {
  public pattern: NodeChild
  public equals: Tok | undefined
  public value: NodeChild
  constructor(pattern: NodeChild, equals: Tok | undefined, value: NodeChild) {
    super([], Tree.Type.Assignment)
    this.pattern = pattern
    this.equals = equals
    this.value = value
  }
  static new(nodes: NodeMap, name: string, expression: AstId): Assignment {
    const pattern = { node: { code: name } }
    const value = { node: expression }
    return new Assignment(pattern, undefined, value)
  }

  get children(): NodeChild[] {
    const defaultEquals = { whitespace: this.value.whitespace, code: '=' }
    return [this.pattern, { node: this.equals ?? defaultEquals }, this.value]
  }
}
export class RawCode extends AbstractNode {
  constructor(code: NodeChild) {
    super([code])
  }

  static new(nodes: NodeMap, code: string): RawCode {
    return new RawCode({ node: { code } })
  }
}
export class Tombstone extends AbstractNode {
  constructor() {
    super()
  }

  static new(): Tombstone {
    return new Tombstone()
  }
}

// edits:
// - child change: replace child by ID
// - children list: shape changes are transactional
//   - this will enable prevention of some bad merges (conflicting positional arg insertions),
//     when combined with arg unrolling
//     - we can implement arg unrolling in the sync repr...
// - edit token contents
//   - print/parse propagates syntax changes to atomic-replacement higher up the tree
//     (set value to more than one token, or set to something that affects the parent)
// * commit edits

// edited subtrees are not automatically reanalyzed until they're committed
// - I think usually an edit won't depend on analysis of the result of some sub-edit
// - we can support explicit reanalysis-points when needed

export type NodeMap = Map<AstId, AbstractNode>

export type CodeMaybePrinted = {
  info?: InfoMap
  code: string
}

export function abstract(
  tree: Tree,
  nodesOut: NodeMap,
  source: CodeMaybePrinted,
): { whitespace: string | undefined; node: AstId } {
  const nodesExpected = new Map(
    Array.from(source.info?.nodes.entries() ?? [], ([span, ids]) => [span, [...ids]]),
  )
  const tokenIds = source.info?.tokens ?? new Map()
  return abstract_(tree, nodesOut, source.code, nodesExpected, tokenIds)
}
function abstract_(
  tree: Tree,
  nodesOut: NodeMap,
  code: string,
  nodesExpected: NodeSpanMap,
  tokenIds: TokenSpanMap,
): { whitespace: string | undefined; node: AstId } {
  const treeType = tree.type
  let node
  const visitChildren = (tree: LazyObject) => {
    const children: NodeChild[] = []
    const visitor = (child: LazyObject) => {
      if (Tree.isInstance(child)) {
        children.push(abstract_(child, nodesOut, code, nodesExpected, tokenIds))
      } else if (Token.isInstance(child)) {
        children.push(abstractToken(child, code, tokenIds))
      } else {
        child.visitChildren(visitor)
      }
    }
    tree.visitChildren(visitor)
    return children
  }
  switch (tree.type) {
    case Tree.Type.BodyBlock:
      const lines = Array.from(tree.statements, (line) => {
        const newline = abstractToken(line.newline, code, tokenIds)
        let expression = null
        if (line.expression != null) {
          expression = abstract_(line.expression, nodesOut, code, nodesExpected, tokenIds)
        }
        return { newline, expression }
      })
      node = new Block(lines)
      break
    case Tree.Type.Function:
      const name = abstract_(tree.name, nodesOut, code, nodesExpected, tokenIds)
      const args = Array.from(tree.args, (arg) => visitChildren(arg))
      const equals = abstractToken(tree.equals, code, tokenIds)
      const body =
        tree.body !== undefined
          ? abstract_(tree.body, nodesOut, code, nodesExpected, tokenIds)
          : null
      node = Function.new(nodesOut, name, args, equals, body)
      break
    default:
      node = new AbstractNode(visitChildren(tree), treeType)
  }
  const whitespaceStart = tree.whitespaceStartInCodeParsed
  const whitespaceEnd = whitespaceStart + tree.whitespaceLengthInCodeParsed
  const whitespace = code.substring(whitespaceStart, whitespaceEnd)
  const codeStart = whitespaceEnd
  const codeEnd = codeStart + tree.childrenLengthInCodeParsed
  const span = nodeKey(codeStart, codeEnd - codeStart, tree.type)
  const id = nodesExpected.get(span)?.shift() ?? newNodeId()
  // XXX: We could skip adding a node to the overlay if it is unchanged.
  // - Use a smart map type that COWs its values and elides unnecessary changes.
  nodesOut.set(id, node)
  return { node: id, whitespace }
}
function abstractToken(token: Token, code: string, tokenIds: TokenSpanMap) {
  const whitespaceStart = token.whitespaceStartInCodeBuffer
  const whitespaceEnd = whitespaceStart + token.whitespaceLengthInCodeBuffer
  const whitespace = code.substring(whitespaceStart, whitespaceEnd)
  const codeStart = token.startInCodeBuffer
  const codeEnd = codeStart + token.lengthInCodeBuffer
  const tokenCode = code.substring(codeStart, codeEnd)
  const span = tokenKey(codeStart, codeEnd - codeStart, token.type)
  const id = tokenIds.get(span) ?? newTokenId()
  const node = { code: tokenCode, id }
  return { whitespace, node }
}

type NodeKey = string
type TokenKey = string
function nodeKey(start: number, length: number, type: Tree.Type | undefined): NodeKey {
  const type_ = type?.toString() ?? '?'
  return `${start}:${length}:${type_}`
}
function tokenKey(start: number, length: number, type: Token.Type): TokenKey {
  return `${start}:${length}:${type}`
}

type NodeSpanMap = Map<NodeKey, AstId[]>
type TokenSpanMap = Map<TokenKey, TokenId>
export type InfoMap = {
  nodes: Map<NodeKey, AstId[]>
  tokens: Map<TokenKey, AstId[]>
}

export type PrintedSource = {
  info: InfoMap
  code: string
}

export function print(root: AstId | Tok, nodes: NodeMap): PrintedSource {
  const info: InfoMap = {
    nodes: new Map(),
    tokens: new Map(),
  }
  const code = print_(root, nodes, info, 0, '')
  return { info, code }
}
function print_(
  root: AstId | Tok,
  nodes: NodeMap,
  info: InfoMap,
  offset: number,
  indent: string,
): string {
  if (isToken(root)) {
    return root.code
  }
  let node = nodes.get(root)
  if (node == null) throw new Error('missing node?')
  let code = ''
  if (node instanceof Block) {
    for (const line of node.lines) {
      if (
        line.expression?.node != null &&
        !isToken(line.expression.node) &&
        nodes.get(line.expression.node) instanceof Tombstone
      )
        continue
      code += line.newline?.whitespace ?? ''
      code += line.newline?.node.code ?? '\n'
      if (line.expression !== null) {
        code += line.expression.whitespace ?? indent
        code += print_(line.expression.node, nodes, info, offset, indent + '    ')
      }
    }
  } else {
    for (const child of node.children) {
      if (child.node != null && !isToken(child.node) && nodes.get(child.node) instanceof Tombstone)
        continue
      if (child.whitespace != null) {
        code += child.whitespace
      } else if (code.length != 0) {
        code += ' '
      }
      if (isToken(child.node)) {
        code += child.node.code
      } else {
        code += print_(child.node, nodes, info, offset + code.length, indent)
      }
    }
  }
  const span = nodeKey(offset, code.length, node.treeType)
  const infos = info.nodes.get(span)
  if (infos == null) {
    info.nodes.set(span, [root])
  } else {
    infos.push(root)
  }
  return code
}

// Translates to a representation where nodes are arrays and leaves are tokens.
export function debug(id: AstId, nodes: NodeMap): any[] {
  let node = nodes.get(id)
  if (node == null) throw new Error('missing node?')
  return node.children.map((child) => {
    if (isToken(child.node)) {
      return child.node.code
    } else {
      return debug(child.node, nodes)
    }
  })
}

export function normalize(root: AstId, nodes: NodeMap): AstId {
  const printed = print(root, nodes)
  const tree = parseEnso(printed.code)
  return abstract(tree, nodes, printed).node
}

export function functionBlock(name: string, nodes: NodeMap): Block | null {
  for (const [_id, node] of nodes) {
    if (node instanceof Function) {
      if (print(node.name.node, nodes).code === name && node.body != null) {
        const bodyId = node.body.node
        if (bodyId !== undefined && !isToken(bodyId)) {
          const body = nodes.get(bodyId)
          if (body instanceof Block) return body
        }
      }
    }
  }
  return null
}

export function insertNewNodeAST(
  block: Block,
  nodes: NodeMap,
  ident: string,
  expression: string,
): { assignment: AstId; value: AstId } {
  const value = newNodeId()
  nodes.set(value, RawCode.new(nodes, expression))
  const assignment = newNodeId()
  nodes.set(assignment, Assignment.new(nodes, ident, value))
  block.lines.push({ expression: { node: assignment } })
  return { assignment, value }
}

export function deleteExpressionAST(nodes: NodeMap, id: AstId) {
  nodes.set(id, new Tombstone())
}

export function replaceExpressionContentAST(nodes: NodeMap, id: AstId, code: string) {
  nodes.set(id, RawCode.new(nodes, code))
}
