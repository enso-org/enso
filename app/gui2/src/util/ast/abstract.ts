import { Token, Tree } from '@/generated/ast'
import { parseEnso } from '@/util/ast'
import { AstExtended as RawAstExtended } from '@/util/ast/extended'
import type { LazyObject } from '@/util/parserSupport'
import { Err, Ok, type Result } from '@/util/result'
import * as random from 'lib0/random'
import { reactive } from 'vue'
import type { ExprId } from '../../../shared/yjsModel'
import { IdMap } from '../../../shared/yjsModel'

const committed = reactive(new Map<AstId, Ast>())
/** New nodes, COW-copies of modified nodes, and pending deletions (nulls) */
const edited = new Map<AstId, Ast | null>()

const astExtended = reactive(new Map<AstId, RawAstExtended>())

export type NodeChild<T = AstId | Tok> = { whitespace?: string | undefined; node: T }

/** Replace all committed values with the state of the uncommitted parse. */
export function syncCommittedFromEdited() {
  for (const id of committed.keys()) {
    if (!edited.has(id)) committed.delete(id)
  }
  for (const [id, ast] of edited.entries()) {
    if (ast === null) {
      committed.delete(id)
    } else {
      committed.set(id, ast)
    }
  }
  edited.clear()
}

/** Returns a syntax node representing the current committed state of the given ID. */
function getNode(id: AstId): Ast | null {
  return committed.get(id) ?? null
}

/** Returns a syntax node representing the current state of the given ID, including any uncommitted modifications. */
function getUncommitted(id: AstId): Ast | null {
  const editedNode = edited.get(id)
  if (editedNode === null) {
    return null
  } else {
    return editedNode ?? committed.get(id) ?? null
  }
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

export class Tok {
  private _code: string
  exprId: TokenId
  readonly _tokenType: Token.Type
  constructor(code: string, id: TokenId, type: Token.Type) {
    this._code = code
    this.exprId = id
    this._tokenType = type
  }

  code(): string {
    return this._code
  }

  typeName(): string {
    return Token.typeNames[this._tokenType]!
  }
}

export abstract class Ast {
  readonly treeType: Tree.Type | undefined
  _id: AstId

  // Deprecated interface for incremental integration of Ast API. Eliminate usages for #8367.
  get astExtended(): RawAstExtended | undefined {
    return astExtended.get(this._id)
  }

  get exprId(): AstId {
    return this._id
  }

  /** Returns child subtrees, without information about the whitespace between them. */
  *children(): IterableIterator<Ast | Tok> {
    for (const child of this._rawChildren()) {
      if (child.node instanceof Tok) {
        yield child.node
      } else {
        const node = getNode(child.node)
        if (node) yield node
      }
    }
  }

  abstract _rawChildren(): IterableIterator<NodeChild>

  code(): string {
    return print(this).code
  }

  typeName(): string | undefined {
    if (this.treeType === undefined) return undefined
    return Tree.typeNames[this.treeType]
  }

  static parse(source: PrintedSource | string): Ast {
    const code = typeof source === 'object' ? source.code : source
    const ids = typeof source === 'object' ? source.info : undefined
    const tree = parseEnso(code)
    return getUncommitted(abstract(tree, code, ids).node)!
  }

  visitRecursive(visit: (node: Ast | Tok) => void) {
    visit(this)
    for (const child of this._rawChildren()) {
      if (child.node instanceof Tok) {
        visit(child.node)
      } else {
        getNode(child.node)?.visitRecursive(visit)
      }
    }
  }

  protected constructor(id?: AstId, treeType?: Tree.Type) {
    this._id = id ?? newNodeId()
    this.treeType = treeType
    edited.set(this._id, this)
  }

  _print(info: InfoMap, offset: number, indent: string): string {
    let code = ''
    for (const child of this._rawChildren()) {
      if (child.node != null && !(child.node instanceof Tok) && getUncommitted(child.node) === null)
        continue
      if (child.whitespace != null) {
        code += child.whitespace
      } else if (code.length != 0) {
        // TODO for #8367: Identify cases where a space should not be inserted.
        code += ' '
      }
      if (child.node != null) {
        if (child.node instanceof Tok) {
          code += child.node.code()
        } else {
          code += getUncommitted(child.node)!._print(info, offset + code.length, indent)
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

  // TODO: Editing (#8367).
  /*
  setExprId(exprId: AstId) {
    this.delete()
    this._id = exprId
    edited.set(this._id, this)
  }

  delete() {
    edited.set(this._id, null)
  }
   */
}

export class App extends Ast {
  private _func: NodeChild<AstId>
  private _leftParen: NodeChild<Tok> | null
  private _argumentName: NodeChild<Tok> | null
  private _equals: NodeChild<Tok> | null
  private _arg: NodeChild<AstId>
  private _rightParen: NodeChild<Tok> | null

  get function(): Ast {
    return getNode(this._func.node)!
  }

  get argumentName(): Tok | null {
    return this._argumentName?.node ?? null
  }

  get argument(): Ast {
    return getNode(this._arg.node)!
  }

  constructor(
    id: AstId | undefined,
    func: NodeChild<AstId>,
    leftParen: NodeChild<Tok> | null,
    name: NodeChild<Tok> | null,
    equals: NodeChild<Tok> | null,
    arg: NodeChild<AstId>,
    rightParen: NodeChild<Tok> | null,
    treeType: Tree.Type,
  ) {
    super(id, treeType)
    this._func = func
    this._leftParen = leftParen
    this._argumentName = name
    this._equals = equals
    this._arg = arg
    this._rightParen = rightParen
  }

  *_rawChildren(): IterableIterator<NodeChild> {
    yield this._func
    if (this._leftParen) yield this._leftParen
    if (this._argumentName) yield this._argumentName
    if (this._equals)
      yield { whitespace: this._equals.whitespace ?? this._arg.whitespace, node: this._equals.node }
    yield this._arg
    if (this._rightParen) yield this._rightParen
  }
}

function positionalApp(
  id: AstId | undefined,
  func: NodeChild<AstId>,
  arg: NodeChild<AstId>,
): App {
  return new App(
    id,
    func,
    null,
    null,
    null,
    arg,
    null,
    getNode(arg.node)?.code() === 'default' ? Tree.Type.DefaultApp : Tree.Type.App,
  )
}

function namedApp(
  id: AstId | undefined,
  func: NodeChild<AstId>,
  leftParen: NodeChild<Tok> | null,
  name: NodeChild<Tok>,
  equals: NodeChild<Tok>, // Edits (#8367): NodeChild<Tok> | undefined
  arg: NodeChild<AstId>,
  rightParen: NodeChild<Tok> | null,
) {
  return new App(id, func, leftParen, name, equals, arg, rightParen, Tree.Type.NamedApp)
}

export class UnaryOprApp extends Ast {
  private _opr: NodeChild<Tok>
  private _arg: NodeChild<AstId> | null

  get operator(): Tok {
    return this._opr.node
  }

  get argument(): Ast | null {
    const id = this._arg?.node
    return id ? getNode(id) : null
  }

  constructor(
    id: AstId | undefined,
    opr: NodeChild<Tok>,
    arg: NodeChild<AstId> | null,
  ) {
    super(id, Tree.Type.UnaryOprApp)
    this._opr = opr
    this._arg = arg
  }

  *_rawChildren(): IterableIterator<NodeChild> {
    yield this._opr
    if (this._arg) yield this._arg
  }
}

export class NegationOprApp extends UnaryOprApp {
  constructor(
    id: AstId | undefined,
    opr: NodeChild<Tok>,
    arg: NodeChild<AstId> | null,
  ) {
    super(id, opr, arg)
  }
}

export class OprApp extends Ast {
  protected _lhs: NodeChild<AstId> | null
  protected _opr: NodeChild[]
  protected _rhs: NodeChild<AstId> | null

  get lhs(): Ast | null {
    return this._lhs ? getNode(this._lhs.node) : null
  }

  get operator(): Result<Tok, NodeChild[]> {
    const first = this._opr[0]?.node
    if (first && this._opr.length < 2 && first instanceof Tok) {
      return Ok(first)
    } else {
      return Err(this._opr)
    }
  }

  get rhs(): Ast | null {
    return this._rhs ? getNode(this._rhs.node) : null
  }

  constructor(
    id: AstId | undefined,
    lhs: NodeChild<AstId> | null,
    opr: NodeChild[],
    rhs: NodeChild<AstId> | null,
  ) {
    super(id, Tree.Type.OprApp)
    this._lhs = lhs
    this._opr = opr
    this._rhs = rhs
  }

  *_rawChildren(): IterableIterator<NodeChild> {
    if (this._lhs) yield this._lhs
    for (const opr of this._opr) yield opr
    if (this._rhs) yield this._rhs
  }
}

export class PropertyAccess extends OprApp {
  constructor(
    id: AstId | undefined,
    lhs: NodeChild<AstId> | null,
    opr: NodeChild<Tok>,
    rhs: NodeChild<AstId> | null,
  ) {
    super(id, lhs, [opr], rhs)
  }
}

/** Representation without any type-specific accessors, for tree types that don't require any special treatment. */
export class Generic extends Ast {
  private readonly _children: NodeChild[]

  constructor(id?: AstId, children?: NodeChild[], treeType?: Tree.Type) {
    super(id, treeType)
    this._children = children ?? []
  }

  _rawChildren(): IterableIterator<NodeChild> {
    return this._children.values()
  }
}

export class NumericLiteral extends Ast {
  private readonly _tokens: NodeChild[]

  constructor(id: AstId | undefined, tokens: NodeChild[]) {
    super(id, Tree.Type.Number)
    this._tokens = tokens ?? []
  }

  _rawChildren(): IterableIterator<NodeChild> {
    return this._tokens.values()
  }
}

type FunctionArgument = NodeChild[]
export class Function extends Ast {
  private _name: NodeChild<AstId>
  private _args: FunctionArgument[]
  private _equals: NodeChild<Tok>
  private _body: NodeChild<AstId> | null
  // FIXME for #8367: This should not be nullable. If the `ExprId` has been deleted, the same placeholder logic should be applied
  //  here and in `rawChildren` (and indirectly, `print`).
  get name(): Ast | null {
    return getNode(this._name.node)
  }
  get body(): Ast | null {
    return this._body ? getNode(this._body.node) : null
  }
  *bodyExpressions(): IterableIterator<Ast> {
    const body = this._body ? getNode(this._body.node) : null
    if (body instanceof Block) {
      yield* body.expressions()
    } else if (body !== null) {
      yield body
    }
  }
  constructor(
    id: AstId | undefined,
    name: NodeChild<AstId>,
    args: FunctionArgument[],
    equals: NodeChild<Tok>, // Edits (#8367): NodeChild<Tok> | undefined
    body: NodeChild<AstId> | null,
  ) {
    super(id, Tree.Type.Function)
    this._name = name
    this._args = args
    this._equals = equals
    this._body = body
  }
  *_rawChildren(): IterableIterator<NodeChild> {
    yield this._name
    for (const arg of this._args) yield* arg
    yield { whitespace: this._equals.whitespace ?? ' ', node: this._equals.node }
    if (this._body !== null) {
      yield this._body
    }
  }
}

export class Assignment extends Ast {
  private _pattern: NodeChild<AstId>
  private _equals: NodeChild<Tok>
  private _expression: NodeChild<AstId>
  get pattern(): Ast | null {
    return getNode(this._pattern.node)
  }
  get expression(): Ast | null {
    return getNode(this._expression.node)
  }
  constructor(
    id: AstId | undefined,
    pattern: NodeChild<AstId>,
    equals: NodeChild<Tok>, // TODO: Edits (#8367): Allow undefined
    expression: NodeChild<AstId>,
  ) {
    super(id, Tree.Type.Assignment)
    this._pattern = pattern
    this._equals = equals
    this._expression = expression
  }
  // TODO: Edits (#8367)
  /*
  static new(
    id: AstId | undefined,
    ident: string,
    equals: WithWhitespace<Tok> | undefined,
    expression: WithWhitespace<AstId>,
  ): Assignment {
    const pattern = { node: Ident.new(ident)._id }
    return new Assignment(id, pattern, equals, expression)
  }
   */
  *_rawChildren(): IterableIterator<NodeChild> {
    yield this._pattern
    yield { whitespace: this._equals.whitespace ?? ' ', node: this._equals.node }
    if (this._expression !== null) {
      yield this._expression
    }
  }
}

type BlockLine = {
  newline: NodeChild<Tok> // Edits (#8367): Allow undefined
  expression: NodeChild<AstId> | null
}
export class Block extends Ast {
  private _lines: BlockLine[];

  *expressions(): IterableIterator<Ast> {
    for (const line of this._lines) {
      if (line.expression) {
        const node = getNode(line.expression.node)
        if (node) {
          yield node
        } else {
          console.warn(`Missing node:`, line.expression.node)
        }
      }
    }
  }

  constructor(id: AstId | undefined, lines: BlockLine[]) {
    super(id, Tree.Type.BodyBlock)
    this._lines = lines
  }

  // TODO: Edits (#8367)
  /*
  static new(id: AstId | undefined, expressions: Ast[]): Block {
    return new Block(
      id,
      expressions.map((e) => ({ expression: { node: e._id } })),
    )
  }

  pushExpression(node: Ast) {
    if (!edited.has(this._id)) edited.set(this._id, new Block(this._id, [...this._lines]))
    const cow = edited.get(this._id) as Block
    cow._lines.push({ expression: { node: node._id } })
  }
   */

  *_rawChildren(): IterableIterator<NodeChild> {
    for (const line of this._lines) {
      yield line.newline
      if (line.expression !== null) yield line.expression
    }
  }

  _print(info: InfoMap, offset: number, indent: string): string {
    let code = ''
    for (const line of this._lines) {
      if (line.expression?.node != null && getUncommitted(line.expression.node) === null) continue
      code += line.newline?.whitespace ?? ''
      code += line.newline?.node.code() ?? '\n'
      if (line.expression !== null) {
        code += line.expression.whitespace ?? indent
        if (line.expression.node !== null) {
          code += getUncommitted(line.expression.node)!._print(info, offset, indent + '    ')
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
  public token: NodeChild<Tok>

  constructor(id: AstId | undefined, token: NodeChild<Tok>) {
    super(id, Tree.Type.Ident)
    this.token = token
  }

  // TODO: Edits (#8367)
  /*
  static new(id: AstId | undefined, code: string): Ident {
    return Ident.fromToken(id, { node: token(code, Token.Type.Ident) })
  }
   */

  *_rawChildren(): IterableIterator<NodeChild> {
    yield this.token
  }
}

export class Wildcard extends Ast {
  public token: NodeChild<Tok>

  constructor(id: AstId | undefined, token: NodeChild<Tok>) {
    super(id, Tree.Type.Wildcard)
    this.token = token
  }

  *_rawChildren(): IterableIterator<NodeChild> {
    yield this.token
  }
}

// TODO: Edits (#8367)
/*
export class RawCode extends Ast {
  private _code: NodeChild

  constructor(id: AstId | undefined, code: NodeChild) {
    super(span, id)
    this._code = code
  }

  static new(id: AstId | undefined, code: string): RawCode {
    return new RawCode(id, { node: token(code, Token.Type.Ident) })
  }

  *_rawChildren(): IterableIterator<NodeChild> {
    yield this._code
  }
}
 */

function abstract(
  tree: Tree,
  code: string,
  info: InfoMap | undefined,
): { whitespace: string | undefined; node: AstId } {
  const nodesExpected = new Map(
    Array.from(info?.nodes.entries() ?? [], ([span, ids]) => [span, [...ids]]),
  )
  const tokenIds = info?.tokens ?? new Map()
  return abstractTree(tree, code, nodesExpected, tokenIds)
}
function abstractTree(
  tree: Tree,
  code: string,
  nodesExpected: NodeSpanMap,
  tokenIds: TokenSpanMap,
): { whitespace: string | undefined; node: AstId } {
  const recurseTree = (tree: Tree) => abstractTree(tree, code, nodesExpected, tokenIds)
  const recurseToken = (token: Token.Token) => abstractToken(token, code, tokenIds)
  let node
  const visitChildren = (tree: LazyObject) => {
    const children: NodeChild[] = []
    const visitor = (child: LazyObject) => {
      if (Tree.isInstance(child)) {
        children.push(recurseTree(child))
      } else if (Token.isInstance(child)) {
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
  const codeStart = whitespaceEnd
  const codeEnd = codeStart + tree.childrenLengthInCodeParsed
  // All node types use this value in the same way to obtain the ID type, but each node does so separately because we
  // must pop the tree's span from the ID map *after* processing children.
  const spanKey = nodeKey(codeStart, codeEnd - codeStart, tree.type)
  switch (tree.type) {
    case Tree.Type.BodyBlock: {
      const lines = Array.from(tree.statements, (line) => {
        const newline = recurseToken(line.newline)
        let expression = null
        if (line.expression != null) {
          expression = recurseTree(line.expression)
        }
        return { newline, expression }
      })
      const id = nodesExpected.get(spanKey)?.pop()
      node = new Block(id, lines)
      break
    }
    case Tree.Type.Function: {
      const name = recurseTree(tree.name)
      const args = Array.from(tree.args, (arg) => visitChildren(arg))
      const equals = recurseToken(tree.equals)
      const body =
        tree.body !== undefined ? recurseTree(tree.body) : null
      const id = nodesExpected.get(spanKey)?.pop()
      node = new Function(id, name, args, equals, body)
      break
    }
    case Tree.Type.Ident: {
      const id = nodesExpected.get(spanKey)?.pop()
      node = new Ident(id, recurseToken(tree.token))
      break
    }
    case Tree.Type.Assignment: {
      const pattern = recurseTree(tree.pattern)
      const equals = recurseToken(tree.equals)
      const value = recurseTree(tree.expr)
      const id = nodesExpected.get(spanKey)?.pop()
      node = new Assignment(id, pattern, equals, value)
      break
    }
    case Tree.Type.App: {
      const func = recurseTree(tree.func)
      const arg = recurseTree(tree.arg)
      const id = nodesExpected.get(spanKey)?.pop()
      node = positionalApp(id, func, arg)
      break
    }
    case Tree.Type.DefaultApp: {
      const func = recurseTree(tree.func)
      const token = recurseToken(tree.default)
      const arg = new Ident(undefined, token).exprId
      const id = nodesExpected.get(spanKey)?.pop()
      node = positionalApp(id, func, { whitespace: token.whitespace, node: arg })
      break
    }
    case Tree.Type.NamedApp: {
      const func = recurseTree(tree.func)
      const leftParen = tree.open ? recurseToken(tree.open) : null
      const name = recurseToken(tree.name)
      const equals = recurseToken(tree.equals)
      const arg = recurseTree(tree.arg)
      const rightParen = tree.close ? recurseToken(tree.close) : null
      const id = nodesExpected.get(spanKey)?.pop()
      node = namedApp(id, func, leftParen, name, equals, arg, rightParen)
      break
    }
    case Tree.Type.UnaryOprApp: {
      const opr = recurseToken(tree.opr)
      const arg = tree.rhs ? recurseTree(tree.rhs) : null
      const id = nodesExpected.get(spanKey)?.pop()
      if (opr.node.code() === '-') {
        node = new NegationOprApp(id, opr, arg)
      } else {
        node = new UnaryOprApp(id, opr, arg)
      }
      break
    }
    case Tree.Type.OprApp: {
      const lhs = tree.lhs ? recurseTree(tree.lhs) : null
      const opr = tree.opr.ok
        ? [recurseToken(tree.opr.value)]
        : visitChildren(tree.opr.error.payload)
      const rhs = tree.rhs ? recurseTree(tree.rhs) : null
      const id = nodesExpected.get(spanKey)?.pop()
      if (opr.length === 1 && opr[0]?.node instanceof Tok && opr[0].node.code() === '.') {
        // Propagate inferred type.
        const token = { whitespace: opr[0].whitespace, node: opr[0].node }
        node = new PropertyAccess(id, lhs, token, rhs)
      } else {
        node = new OprApp(id, lhs, opr, rhs)
      }
      break
    }
    case Tree.Type.Number: {
      const id = nodesExpected.get(spanKey)?.pop()
      node = new NumericLiteral(id, visitChildren(tree))
      break
    }
    case Tree.Type.Wildcard: {
      const token = recurseToken(tree.token)
      const id = nodesExpected.get(spanKey)?.pop()
      node = new Wildcard(id, token)
      break
    }
    default: {
      const id = nodesExpected.get(spanKey)?.pop()
      node = new Generic(id, visitChildren(tree), tree.type)
    }
  }
  const whitespace = code.substring(whitespaceStart, whitespaceEnd)
  return { node: node._id, whitespace }
}

function abstractToken(
  token: Token,
  code: string,
  tokenIds: TokenSpanMap,
): { whitespace: string; node: Tok } {
  const whitespaceStart = token.whitespaceStartInCodeBuffer
  const whitespaceEnd = whitespaceStart + token.whitespaceLengthInCodeBuffer
  const whitespace = code.substring(whitespaceStart, whitespaceEnd)
  const codeStart = token.startInCodeBuffer
  const codeEnd = codeStart + token.lengthInCodeBuffer
  const tokenCode = code.substring(codeStart, codeEnd)
  const span = { start: codeStart, end: codeEnd, whitespaceLength: whitespaceEnd - whitespaceStart }
  const key = tokenKey(codeStart, codeEnd - codeStart)
  const exprId = tokenIds.get(key) ?? newTokenId()
  const node = new Tok(tokenCode, exprId, token.type)
  return { whitespace, node }
}

type NodeKey = string
type TokenKey = string
function nodeKey(start: number, length: number, type: Tree.Type | undefined): NodeKey {
  const type_ = type?.toString() ?? '?'
  return `${start}:${length}:${type_}`
}
function tokenKey(start: number, length: number): TokenKey {
  return `${start}:${length}`
}

type NodeSpanMap = Map<NodeKey, AstId[]>
type TokenSpanMap = Map<TokenKey, TokenId>
export type InfoMap = {
  nodes: NodeSpanMap
  tokens: TokenSpanMap
}

type PrintedSource = {
  info: InfoMap
  code: string
}
/** Return stringification with associated ID map. This is only exported for testing. */
export function print(ast: Ast): PrintedSource {
  const info: InfoMap = {
    nodes: new Map(),
    tokens: new Map(),
  }
  const code = ast._print(info, 0, '')
  return { info, code }
}

type DebugTree = (DebugTree | string)[]
export function debug(root: Ast, universe?: Map<AstId, Ast>): DebugTree {
  return Array.from(root._rawChildren(), (child) => {
    if (child.node instanceof Tok) {
      return child.node.code()
    } else {
      const node = (universe ?? edited).get(child.node)
      return node ? debug(node, universe) : '<missing>'
    }
  })
}

// TODO: Edits (#8367)
/*
export function normalize(root: AstId): AstId {
  const printed = print(getNode(root)!)
  const tree = parseEnso(printed.code)
  return abstract(tree, printed.code, printed.info).node
}
 */

// FIXME: We should use alias analysis to handle ambiguous names correctly.
export function findModuleMethod(name: string, universe?: Map<AstId, Ast>): Function | null {
  for (const node of (universe ?? committed).values()) {
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

// TODO for #8367: Use new Ast for edits.
/*
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
 */

/** For use in tests where we never actually synchronize edits to the committed AST. */
export function forgetAllAsts() {
  edited.clear()
  committed.clear()
}

// TODO: Edits (#8367)
/*
export function parse(source: PrintedSource | string): Ast {
  return Ast.parse(source)
}
 */

/** Parse using new AST types with old IdMap synchronization/editing. */
export function parseTransitional(code: string, idMap: IdMap): Ast {
  const legacyAst = RawAstExtended.parse(code, idMap)
  idMap.finishAndSynchronize()
  const nodes = new Map<NodeKey, AstId[]>()
  const tokens = new Map<TokenKey, TokenId>()
  astExtended.clear()
  legacyAst.visitRecursive((nodeOrToken: RawAstExtended<Tree | Token>) => {
    const start = nodeOrToken.span()[0]
    const length = nodeOrToken.span()[1] - nodeOrToken.span()[0]
    if (nodeOrToken.isToken()) {
      const token: RawAstExtended<Token> = nodeOrToken
      tokens.set(tokenKey(start, length), token.astId as TokenId)
    } else if (nodeOrToken.isTree()) {
      const node: RawAstExtended<Tree> = nodeOrToken
      let id = node.astId as AstId
      if (astExtended.has(id)) {
        id = newNodeId()
      }
      astExtended.set(id, nodeOrToken)
      const key = nodeKey(start, length, node.inner.type)
      const ids = nodes.get(key)
      if (ids !== undefined) {
        ids.push(id)
      } else {
        nodes.set(key, [id])
      }
    }
    return true
  })
  const newRoot = Ast.parse({ info: { nodes, tokens }, code })
  syncCommittedFromEdited()
  return newRoot
}

declare const AstKey: unique symbol
declare const TokenKey: unique symbol
declare module '@/providers/widgetRegistry' {
  export interface WidgetInputTypes {
    [AstKey]: Ast
    [TokenKey]: Tok
  }
}
