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

type NodeMap = Map<AstId, Ast>

export abstract class Ast {
  public treeType: Tree.Type | undefined
  public id: AstId

  protected constructor(id?: AstId, treeType?: Tree.Type) {
    this.id = id ?? newNodeId()
    this.treeType = treeType
  }

  abstract children(): Iterable<NodeChild>

  print(nodes: NodeMap): PrintedSource {
    const info: InfoMap = {
      nodes: new Map(),
      tokens: new Map(),
    }
    const code = this.print_(nodes, info, 0, '')
    return { info, code }
  }

  print_(
    nodes: NodeMap,
    info: InfoMap,
    offset: number,
    indent: string,
  ): string {
    let code = ''
    for (const child of this.children()) {
      if (child.node != null && !isToken(child.node) && nodes.get(child.node) instanceof Tombstone)
        continue
      if (child.whitespace != null) {
        code += child.whitespace
      } else if (code.length != 0) {
        code += ' '
      }
      if (child.node != null) {
        if (isToken(child.node)) {
          code += child.node.code
        } else {
          code += nodes.get(child.node)!.print_(nodes, info, offset + code.length, indent)
        }
      }
    }
    const span = nodeKey(offset, code.length, this.treeType)
    const infos = info.nodes.get(span)
    if (infos == null) {
      info.nodes.set(span, [this.id])
    } else {
      infos.push(this.id)
    }
    return code
  }
}

export class Generic extends Ast {
  private readonly _children: NodeChild[]

  protected constructor(id?: AstId, children?: NodeChild[], treeType?: Tree.Type) {
    super(id, treeType)
    this._children = children ?? []
  }

  static new(nodes: NodeMap, id?: AstId, children?: NodeChild[], treeType?: Tree.Type) {
    const node = new Generic(id, children, treeType)
    nodes.set(node.id, node)
    return node
  }

  children(): Iterable<NodeChild> {
    return this._children
  }
}

type FunctionArgument = NodeChild[]
type TokWithWhitespace = { whitespace?: string; node: Tok }
export class Function extends Ast {
  public name: NodeChild
  public args: FunctionArgument[]
  public equals: TokWithWhitespace | undefined
  public body: NodeChild | null
  protected constructor(
    id: AstId | undefined,
    name: NodeChild,
    args: FunctionArgument[],
    equals: TokWithWhitespace | undefined,
    body: NodeChild | null,
  ) {
    super(id, Tree.Type.Function)
    this.name = name
    this.args = args
    this.equals = equals
    this.body = body
  }
  static new(
    nodes: NodeMap,
    id: AstId | undefined,
    name: NodeChild,
    args: FunctionArgument[],
    equals: TokWithWhitespace | undefined,
    body: NodeChild | null,
  ): Function {
    const node = new Function(id, name, args, equals, body)
    nodes.set(node.id, node)
    return node
  }
  *children(): Iterable<NodeChild> {
    yield this.name
    for (const arg of this.args)
      yield *arg
    if (this.equals !== undefined) {
      yield { whitespace: this.equals.whitespace ?? ' ', node: this.equals.node }
    } else {
      yield { whitespace: ' ', node: { code: '=' } }
    }
    if (this.body !== null) {
      yield this.body
    }
  }
}

type BlockLine = {
  newline?: TokWithWhitespace
  expression: NodeChild | null
}
export class Block extends Ast {
  public lines: BlockLine[]

  protected constructor(id: AstId | undefined, lines: BlockLine[]) {
    super(id, Tree.Type.BodyBlock)
    this.lines = lines
  }

  static new(nodes: NodeMap, id: AstId | undefined, lines: BlockLine[]): Block {
    const node = new Block(id, lines)
    nodes.set(node.id, node)
    return node
  }

  *children(): Iterable<NodeChild> {
    for (const line of this.lines) {
      yield line.newline ?? { node: { code: '\n' } }
      if (line.expression !== null) yield line.expression
    }
  }

  print_(
    nodes: NodeMap,
    info: InfoMap,
    offset: number,
    indent: string,
  ): string {
    let code = ''
    for (const line of this.lines) {
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
        if (line.expression.node != null) {
          if (isToken(line.expression.node)) {
            code += line.expression.node.code
          } else {
            code += nodes.get(line.expression.node)!.print_(nodes, info, offset, indent + '    ')
          }
        }
      }
    }
    const span = nodeKey(offset, code.length, this.treeType)
    const infos = info.nodes.get(span)
    if (infos == null) {
      info.nodes.set(span, [this.id])
    } else {
      infos.push(this.id)
    }
    return code
  }
}

export class Assignment extends Ast {
  public pattern: NodeChild
  public equals: Tok | undefined
  public value: NodeChild
  protected constructor(id: AstId | undefined, pattern: NodeChild, equals: Tok | undefined, value: NodeChild) {
    super(id, Tree.Type.Assignment)
    this.pattern = pattern
    this.equals = equals
    this.value = value
  }
  static new(nodes: NodeMap, id: AstId | undefined, name: string, expression: AstId): Assignment {
    const pattern = { node: { code: name } }
    const value = { node: expression }
    const node = new Assignment(id, pattern, undefined, value)
    nodes.set(node.id, node)
    return node
  }

  *children(): Iterable<NodeChild> {
    yield this.pattern
    const defaultEquals = { whitespace: this.value.whitespace, code: '=' }
    yield { node: this.equals ?? defaultEquals }
    yield this.value
  }
}
export class RawCode extends Ast {
  public code: NodeChild

  protected constructor(id: AstId | undefined, code: NodeChild) {
    super(id)
    this.code = code
  }

  static new(nodes: NodeMap, id: AstId | undefined, code: string): RawCode {
    const node = new RawCode(id, { node: { code } })
    nodes.set(node.id, node)
    return node
  }

  *children(): Iterable<NodeChild> {
    yield this.code
  }
}
export class Tombstone extends Ast {
  protected constructor(id: AstId | undefined) {
    super(id)
  }

  static new(nodes: NodeMap, id: AstId | undefined): Tombstone {
    const node = new Tombstone(id)
    nodes.set(node.id, node)
    return node
  }

  children(): Iterable<NodeChild> {
    return []
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

export type CodeMaybePrinted = {
  info?: InfoMap
  code: string
}

export function abstract(
  nodesOut: NodeMap,
  tree: Tree,
  source: CodeMaybePrinted,
): { whitespace: string | undefined; node: AstId } {
  const nodesExpected = new Map(
    Array.from(source.info?.nodes.entries() ?? [], ([span, ids]) => [span, [...ids]]),
  )
  const tokenIds = source.info?.tokens ?? new Map()
  return abstract_(nodesOut, tree, source.code, nodesExpected, tokenIds)
}
function abstract_(
  nodesOut: NodeMap,
  tree: Tree,
  code: string,
  nodesExpected: NodeSpanMap,
  tokenIds: TokenSpanMap,
): { whitespace: string | undefined; node: AstId } {
  let node
  const visitChildren = (tree: LazyObject) => {
    const children: NodeChild[] = []
    const visitor = (child: LazyObject) => {
      if (Tree.isInstance(child)) {
        children.push(abstract_(nodesOut, child, code, nodesExpected, tokenIds))
      } else if (Token.isInstance(child)) {
        children.push(abstractToken(child, code, tokenIds))
      } else {
        child.visitChildren(visitor)
      }
    }
    tree.visitChildren(visitor)
    return children
  }
  const whitespaceStart = tree.whitespaceStartInCodeParsed
  const whitespaceEnd = whitespaceStart + tree.whitespaceLengthInCodeParsed
  const codeStart = whitespaceEnd
  const codeEnd = codeStart + tree.childrenLengthInCodeParsed
  const span = nodeKey(codeStart, codeEnd - codeStart, tree.type)
  switch (tree.type) {
    case Tree.Type.BodyBlock: {
      const lines = Array.from(tree.statements, (line) => {
        const newline = abstractToken(line.newline, code, tokenIds)
        let expression = null
        if (line.expression != null) {
          expression = abstract_(nodesOut, line.expression, code, nodesExpected, tokenIds)
        }
        return { newline, expression }
      })
      const id = nodesExpected.get(span)?.pop()
      node = Block.new(nodesOut, id, lines)
      break
    }
    case Tree.Type.Function: {
      const name = abstract_(nodesOut, tree.name, code, nodesExpected, tokenIds)
      const args = Array.from(tree.args, (arg) => visitChildren(arg))
      const equals = abstractToken(tree.equals, code, tokenIds)
      const body =
        tree.body !== undefined
          ? abstract_(nodesOut, tree.body, code, nodesExpected, tokenIds)
          : null
      const id = nodesExpected.get(span)?.pop()
      node = Function.new(nodesOut, id, name, args, equals, body)
      break
    }
    default: {
      const id = nodesExpected.get(span)?.pop()
      node = Generic.new(nodesOut, id, visitChildren(tree), tree.type)
    }
  }
  const whitespace = code.substring(whitespaceStart, whitespaceEnd)
  return { node: node.id, whitespace }
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

type DebugTree = (DebugTree | string)[]
export function debug(nodes: NodeMap, id: AstId): DebugTree {
  return Array.from(nodes.get(id)!.children(), (child) => {
    if (isToken(child.node)) {
      return child.node.code
    } else {
      return debug(nodes, child.node)
    }
  })
}

export function normalize(nodes: NodeMap, root: AstId): AstId {
  const printed = nodes.get(root)!.print(nodes)
  const tree = parseEnso(printed.code)
  return abstract(nodes, tree, printed).node
}

export function functionBlock(nodes: NodeMap, name: string): Block | null {
  for (const [_id, node] of nodes) {
    if (node instanceof Function) {
      const nodeName = (isToken(node.name.node)) ? node.name.node.code : nodes.get(node.name.node)!.print(nodes)
      if (nodeName === name) {
        if (node.body != null) {
          const bodyId = node.body.node
          if (bodyId !== undefined && !isToken(bodyId)) {
            const body = nodes.get(bodyId)
            if (body instanceof Block) return body
          }
        }
      }
    }
  }
  return null
}

export function insertNewNodeAST(
  nodes: NodeMap,
  block: Block,
  ident: string,
  expression: string,
): { assignment: AstId; value: AstId } {
  const value = RawCode.new(nodes, undefined, expression)
  const assignment = Assignment.new(nodes, undefined, ident, value.id)
  block.lines.push({ expression: { node: assignment.id } })
  return { assignment: assignment.id, value: value.id }
}

export function deleteExpressionAST(nodes: NodeMap, id: AstId) {
  return Tombstone.new(nodes, id)
}

export function replaceExpressionContentAST(nodes: NodeMap, id: AstId, code: string) {
  return RawCode.new(nodes, id, code)
}