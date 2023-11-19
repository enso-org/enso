/**
 * # AST
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
import { reactive } from 'vue'
import type { ExprId } from "../../../shared/yjsModel";
import * as random from "lib0/random";

const committed = reactive(new Map<AstId, Ast>())
const edited = new Map<AstId, Ast | null>()

function getNode(id: AstId): Ast | null {
  return committed.get(id) ?? edited.get(id) ?? null
}

function getUncommitted(id: AstId): Ast | null {
  return edited.get(id) ?? committed.get(id) ?? null
}

declare const brandAstId: unique symbol
declare const brandTokenId: unique symbol
export type AstId = ExprId & { [brandAstId]: never }
export type TokenId = ExprId & { [brandTokenId]: never }

function newNodeId(): AstId {
  return random.uuidv4() as AstId
}
function newTokenId(): TokenId {
  return random.uuidv4() as TokenId
}

export type Tok = { code: string; exprId: TokenId }
export type NodeChild = { whitespace?: string | undefined; node: AstId | Tok }
type AstWithWhitespace = { whitespace?: string | undefined; node: AstId }
type TokWithWhitespace = { whitespace?: string; node: Tok }

function token(code: string): Tok {
  return { code, exprId: newTokenId() }
}

export function isToken(node: Tok | AstId): node is Tok {
  return typeof node === 'object'
}

// Ast:
// - If persisted a reactive watcher should replace it when necessary. (Simpler to persist ExprId).
// - Synchronization will sometimes keep it up to date but this should not be relied on.
//   - Certain types of change can orphan it, and it will no longer receive updates.
//     - (Orphaning is an effect, though.)

// ExprId: Stable identifier. Untyped (type of associated Ast can change).

export abstract class Ast {
  protected readonly treeType: Tree.Type | undefined
  public _id: AstId

  protected constructor(id?: AstId, treeType?: Tree.Type) {
    this._id = id ?? newNodeId()
    this.treeType = treeType
    edited.set(this._id, this)
  }

  get exprId(): AstId {
    return this._id
  }

  setExprId(exprId: AstId) {
    this.delete()
    this._id = exprId
    edited.set(this._id, this)
  }

  delete() {
    edited.set(this._id, null)
  }

  abstract children(): Iterable<NodeChild>

  code(): string {
    return this.print().code
  }

  print(): PrintedSource {
    const info: InfoMap = {
      nodes: new Map(),
      tokens: new Map(),
    }
    const code = this.print_(info, 0, '')
    return { info, code }
  }

  print_(info: InfoMap, offset: number, indent: string): string {
    let code = ''
    for (const child of this.children()) {
      if (child.node != null && !isToken(child.node) && getUncommitted(child.node) === null)
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
          code += getUncommitted(child.node)!.print_(info, offset + code.length, indent)
        }
      }
    }
    const span = nodeKey(offset, code.length, this.treeType)
    const infos = info.nodes.get(span)
    if (infos == null) {
      info.nodes.set(span, [this._id])
    } else {
      infos.push(this._id)
    }
    return code
  }

  visitRecursive(visit: (node: Ast | Tok) => void) {
    visit(this)
    for (const child of this.children()) {
      if (isToken(child.node)) {
        visit(child.node)
      } else {
        getNode(child.node)?.visitRecursive(visit)
      }
    }
  }
}

export class Generic extends Ast {
  private readonly _children: NodeChild[]

  constructor(id?: AstId, children?: NodeChild[], treeType?: Tree.Type) {
    super(id, treeType)
    this._children = children ?? []
  }

  static new(id?: AstId, children?: NodeChild[], treeType?: Tree.Type) {
    return new Generic(id, children, treeType)
  }

  children(): Iterable<NodeChild> {
    return this._children
  }
}

type FunctionArgument = NodeChild[]
export class Function extends Ast {
  private _name: AstWithWhitespace
  private _args: FunctionArgument[]
  private _equals: TokWithWhitespace | undefined
  private _body: AstWithWhitespace | null
  // FIXME: This should not be nullable. If the `ExprId` has been deleted, the same placeholder logic should be applied
  //  here and in `children` (and indirectly, `print`).
  get name(): Ast | null {
    return getNode(this._name.node)
  }
  get body(): Ast | null {
    return this._body ? getNode(this._body.node) : null
  }
  *bodyExpressions(): Iterable<Ast> {
    const body = this._body ? getNode(this._body.node) : null
    if (body instanceof Block) {
      yield* body.expressions()
    } else if (body !== null) {
      yield body
    }
  }
  constructor(
    id: AstId | undefined,
    name: AstWithWhitespace,
    args: FunctionArgument[],
    equals: TokWithWhitespace | undefined,
    body: AstWithWhitespace | null,
  ) {
    super(id, Tree.Type.Function)
    this._name = name
    this._args = args
    this._equals = equals
    this._body = body
  }
  static new(
    id: AstId | undefined,
    name: AstWithWhitespace,
    args: FunctionArgument[],
    equals: TokWithWhitespace | undefined,
    body: AstWithWhitespace | null,
  ): Function {
    return new Function(id, name, args, equals, body)
  }
  *children(): Iterable<NodeChild> {
    yield this._name
    for (const arg of this._args) yield* arg
    if (this._equals !== undefined) {
      yield { whitespace: this._equals.whitespace ?? ' ', node: this._equals.node }
    } else {
      yield { whitespace: ' ', node: token('=') }
    }
    if (this._body !== null) {
      yield this._body
    }
  }
}

export class Assignment extends Ast {
  private _pattern: AstWithWhitespace
  private _equals: TokWithWhitespace | undefined
  private _expression: AstWithWhitespace
  get pattern(): Ast | null {
    return getNode(this._pattern.node)
  }
  get expression(): Ast | null {
    return getNode(this._expression.node)
  }
  constructor(
    id: AstId | undefined,
    pattern: AstWithWhitespace,
    equals: TokWithWhitespace | undefined,
    expression: AstWithWhitespace,
  ) {
    super(id, Tree.Type.Assignment)
    this._pattern = pattern
    this._equals = equals
    this._expression = expression
  }
  static new(
    id: AstId | undefined,
    ident: string,
    equals: TokWithWhitespace | undefined,
    expression: AstWithWhitespace,
  ): Assignment {
    const pattern = { node: Ident.new(undefined, ident)._id }
    return new Assignment(id, pattern, equals, expression)
  }
  *children(): Iterable<NodeChild> {
    yield this._pattern
    if (this._equals !== undefined) {
      yield { whitespace: this._equals.whitespace ?? ' ', node: this._equals.node }
    } else {
      yield { whitespace: ' ', node: token('=') }
    }
    if (this._expression !== null) {
      yield this._expression
    }
  }
}

type BlockLine = {
  newline?: TokWithWhitespace
  expression: AstWithWhitespace | null
}
export class Block extends Ast {
  private _lines: BlockLine[];

  *expressions(): Iterable<Ast> {
    for (const line of this._lines) {
      if (line.expression) {
        const node = getNode(line.expression.node)
        if (node) yield node
      }
    }
  }

  pushExpression(node: Ast) {
    if (!edited.has(this._id))
      edited.set(this._id, new Block(this._id, [...this._lines]))
    const cow = edited.get(this._id) as Block
    cow._lines.push({ expression: { node: node._id } })
  }

  constructor(id: AstId | undefined, lines: BlockLine[]) {
    super(id, Tree.Type.BodyBlock)
    this._lines = lines
  }

  static new(id: AstId | undefined, expressions: Ast[]): Block {
    return new Block(id, expressions.map((e) => ({ expression: { node: e._id } })))
  }

  *children(): Iterable<NodeChild> {
    for (const line of this._lines) {
      yield line.newline ?? { node: token('\n') }
      if (line.expression !== null) yield line.expression
    }
  }

  print_(info: InfoMap, offset: number, indent: string): string {
    let code = ''
    for (const line of this._lines) {
      if (
        line.expression?.node != null &&
        !isToken(line.expression.node) &&
        getUncommitted(line.expression.node) === null
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
            code += getUncommitted(line.expression.node)!.print_(info, offset, indent + '    ')
          }
        }
      }
    }
    const span = nodeKey(offset, code.length, this.treeType)
    const infos = info.nodes.get(span)
    if (infos == null) {
      info.nodes.set(span, [this._id])
    } else {
      infos.push(this._id)
    }
    return code
  }
}

export class Ident extends Ast {
  public token: TokWithWhitespace

  constructor(id: AstId | undefined, token: TokWithWhitespace) {
    super(id, Tree.Type.Ident)
    this.token = token
  }

  static new(id: AstId | undefined, code: string): Ident {
    return Ident.fromToken(id, { node: token(code) })
  }

  static fromToken(id: AstId | undefined, token: TokWithWhitespace): Ident {
    return new Ident(id, token)
  }

  *children(): Iterable<NodeChild> {
    yield this.token
  }
}

export class RawCode extends Ast {
  private _code: NodeChild

  constructor(id: AstId | undefined, code: NodeChild) {
    super(id)
    this._code = code
  }

  static new(id: AstId | undefined, code: string): RawCode {
    return new RawCode(id, { node: token(code) })
  }

  *children(): Iterable<NodeChild> {
    yield this._code
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

export function parse(source: PrintedSource | string): Ast {
  const code = typeof source === 'object' ? source.code : source
  const ids = typeof source === 'object' ? source.info : undefined
  const tree = parseEnso(code)
  return getUncommitted(abstract(tree, code, ids).node)!
}

function abstract(
  tree: Tree,
  code: string,
  info: InfoMap | undefined,
): { whitespace: string | undefined; node: AstId } {
  const nodesExpected = new Map(
    Array.from(info?.nodes.entries() ?? [], ([span, ids]) => [span, [...ids]]),
  )
  const tokenIds = info?.tokens ?? new Map()
  return abstract_(tree, code, nodesExpected, tokenIds)
}
function abstract_(
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
        children.push(abstract_(child, code, nodesExpected, tokenIds))
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
          expression = abstract_(line.expression, code, nodesExpected, tokenIds)
        }
        return { newline, expression }
      })
      const id = nodesExpected.get(span)?.pop()
      node = new Block(id, lines)
      break
    }
    case Tree.Type.Function: {
      const name = abstract_(tree.name, code, nodesExpected, tokenIds)
      const args = Array.from(tree.args, (arg) => visitChildren(arg))
      const equals = abstractToken(tree.equals, code, tokenIds)
      const body =
        tree.body !== undefined ? abstract_(tree.body, code, nodesExpected, tokenIds) : null
      const id = nodesExpected.get(span)?.pop()
      node = new Function(id, name, args, equals, body)
      break
    }
    case Tree.Type.Ident: {
      const id = nodesExpected.get(span)?.pop()
      node = Ident.fromToken(id, abstractToken(tree.token, code, tokenIds))
      break
    }
    case Tree.Type.Assignment: {
      const pattern = abstract_(tree.pattern, code, nodesExpected, tokenIds)
      const equals = abstractToken(tree.equals, code, tokenIds)
      const value = abstract_(tree.expr, code, nodesExpected, tokenIds)
      const id = nodesExpected.get(span)?.pop()
      node = new Assignment(id, pattern, equals, value)
      break
    }
    default: {
      const id = nodesExpected.get(span)?.pop()
      node = new Generic(id, visitChildren(tree), tree.type)
    }
  }
  const whitespace = code.substring(whitespaceStart, whitespaceEnd)
  return { node: node._id, whitespace }
}

function abstractToken(token: Token, code: string, tokenIds: TokenSpanMap): { whitespace: string, node: Tok } {
  const whitespaceStart = token.whitespaceStartInCodeBuffer
  const whitespaceEnd = whitespaceStart + token.whitespaceLengthInCodeBuffer
  const whitespace = code.substring(whitespaceStart, whitespaceEnd)
  const codeStart = token.startInCodeBuffer
  const codeEnd = codeStart + token.lengthInCodeBuffer
  const tokenCode = code.substring(codeStart, codeEnd)
  const span = tokenKey(codeStart, codeEnd - codeStart, token.type)
  const exprId = tokenIds.get(span) ?? newTokenId()
  const node = { code: tokenCode, exprId }
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
export function debug(id: AstId): DebugTree {
  return Array.from(getNode(id)!.children(), (child) => {
    if (isToken(child.node)) {
      return child.node.code
    } else {
      return debug(child.node)
    }
  })
}

export function normalize(root: AstId): AstId {
  const printed = getNode(root)!.print()
  const tree = parseEnso(printed.code)
  return abstract(tree, printed.code, printed.info).node
}

export function findModuleMethod(name: string): Function | null {
  for (const node of committed.values()) {
    if (node instanceof Function) {
      if (node.name && node.name.code() === name) {
        return node
      }
    }
  }
  return null
}

export function functionBlock(name: string): Block | null {
  const method = findModuleMethod(name)
  if (!method || !(method.body instanceof Block)) return null
  return method.body
}

export function insertNewNodeAST(
  block: Block,
  ident: string,
  expression: string,
): { assignment: AstId; value: AstId } {
  const value = RawCode.new(undefined, expression)._id
  const assignment = Assignment.new(undefined, ident, undefined, { node: value })
  block.pushExpression(assignment)
  return { assignment: assignment._id, value }
}

export function deleteExpressionAST(id: AstId) {
  edited.set(id, null)
}

export function replaceExpressionContentAST(id: AstId, code: string) {
  return RawCode.new(id, code)
}

export function forgetAllAsts() {
  edited.clear()
  committed.clear()
}
