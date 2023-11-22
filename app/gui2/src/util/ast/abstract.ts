import { Token, Tree } from '@/generated/ast'
import { parseEnso } from '@/util/ast'
import { AstExtended } from '@/util/ast/extended.ts'
import * as Tok from '@/util/ast/token'
import type { LazyObject } from '@/util/parserSupport'
import { Err, Ok, type Result } from '@/util/result'
import * as random from 'lib0/random'
import { reactive } from 'vue'
import type { ExprId } from '../../../shared/yjsModel'
import { IdMap } from '../../../shared/yjsModel'
export { Tok }

const committed = reactive(new Map<AstId, Ast>())
/** New nodes, COW-copies of modified nodes, and pending deletions (nulls) */
const edited = new Map<AstId, Ast | null>()

export type NodeChild<T = AstId | Tok.Tok> = { whitespace?: string | undefined; node: T }

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

export type Span = { start: number; end: number; whitespaceLength: number }
export abstract class Ast {
  readonly treeType: Tree.Type | undefined
  _id: AstId
  // TODO for #8367: Eliminate this before enabling edit support.
  readonly span: Span

  get exprId(): AstId {
    return this._id
  }

  /** Returns child subtrees, without information about the whitespace between them. */
  *children(): IterableIterator<Ast | Tok.Tok> {
    for (const child of this._rawChildren()) {
      if (child.node instanceof Tok.Tok) {
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

  visitRecursive(visit: (node: Ast | Tok.Tok) => void) {
    visit(this)
    for (const child of this._rawChildren()) {
      if (child.node instanceof Tok.Tok) {
        visit(child.node)
      } else {
        getNode(child.node)?.visitRecursive(visit)
      }
    }
  }

  protected constructor(span: Span, id?: AstId, treeType?: Tree.Type) {
    this._id = id ?? newNodeId()
    this.treeType = treeType
    this.span = span
    edited.set(this._id, this)
  }

  _print(info: InfoMap, offset: number, indent: string): string {
    let code = ''
    for (const child of this._rawChildren()) {
      if (
        child.node != null &&
        !(child.node instanceof Tok.Tok) &&
        getUncommitted(child.node) === null
      )
        continue
      if (child.whitespace != null) {
        code += child.whitespace
      } else if (code.length != 0) {
        // TODO for #8367: Identify cases where a space should not be inserted.
        code += ' '
      }
      if (child.node != null) {
        if (child.node instanceof Tok.Tok) {
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
  private _leftParen: NodeChild<Tok.OpenSymbol> | null
  private _argumentName: NodeChild<Tok.Ident> | null
  private _equals: NodeChild<Tok.Operator> | null
  private _arg: NodeChild<AstId>
  private _rightParen: NodeChild<Tok.CloseSymbol> | null

  get function(): Ast {
    return getNode(this._func.node)!
  }

  get argumentName(): Tok.Ident | null {
    return this._argumentName?.node ?? null
  }

  get argument(): Ast {
    return getNode(this._arg.node)!
  }

  constructor(
    span: Span,
    id: AstId | undefined,
    func: NodeChild<AstId>,
    leftParen: NodeChild<Tok.OpenSymbol> | null,
    name: NodeChild<Tok.Ident> | null,
    equals: NodeChild<Tok.Operator> | null,
    arg: NodeChild<AstId>,
    rightParen: NodeChild<Tok.CloseSymbol> | null,
    treeType: Tree.Type,
  ) {
    super(span, id, treeType)
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
  span: Span,
  id: AstId | undefined,
  func: NodeChild<AstId>,
  arg: NodeChild<AstId>,
): App {
  return new App(
    span,
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
  span: Span,
  id: AstId | undefined,
  func: NodeChild<AstId>,
  leftParen: NodeChild<Tok.OpenSymbol> | null,
  name: NodeChild<Tok.Ident>,
  equals: NodeChild<Tok.Operator>, // Edits (#8367): NodeChild<Tok.Tok> | undefined
  arg: NodeChild<AstId>,
  rightParen: NodeChild<Tok.CloseSymbol> | null,
) {
  return new App(span, id, func, leftParen, name, equals, arg, rightParen, Tree.Type.NamedApp)
}

export class UnaryOprApp extends Ast {
  private _opr: NodeChild<Tok.Operator>
  private _arg: NodeChild<AstId> | null

  get operator(): Tok.Operator {
    return this._opr.node
  }

  get argument(): Ast | null {
    const id = this._arg?.node
    return id ? getNode(id) : null
  }

  constructor(
    span: Span,
    id: AstId | undefined,
    opr: NodeChild<Tok.Operator>,
    arg: NodeChild<AstId> | null,
  ) {
    super(span, id, Tree.Type.UnaryOprApp)
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
    span: Span,
    id: AstId | undefined,
    opr: NodeChild<Tok.Operator>,
    arg: NodeChild<AstId> | null,
  ) {
    super(span, id, opr, arg)
  }
}

export class OprApp extends Ast {
  protected _lhs: NodeChild<AstId> | null
  protected _opr: NodeChild[]
  protected _rhs: NodeChild<AstId> | null

  get lhs(): Ast | null {
    return this._lhs ? getNode(this._lhs.node) : null
  }

  get operator(): Result<Tok.Operator, NodeChild[]> {
    const first = this._opr[0]?.node
    if (first && this._opr.length < 2 && first instanceof Tok.Operator) {
      return Ok(first)
    } else {
      return Err(this._opr)
    }
  }

  get rhs(): Ast | null {
    return this._rhs ? getNode(this._rhs.node) : null
  }

  constructor(
    span: Span,
    id: AstId | undefined,
    lhs: NodeChild<AstId> | null,
    opr: NodeChild[],
    rhs: NodeChild<AstId> | null,
  ) {
    super(span, id, Tree.Type.OprApp)
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
    span: Span,
    id: AstId | undefined,
    lhs: NodeChild<AstId> | null,
    opr: NodeChild<Tok.Operator>,
    rhs: NodeChild<AstId> | null,
  ) {
    super(span, id, lhs, [opr], rhs)
  }
}

/** Representation without any type-specific accessors, for tree types that don't require any special treatment. */
export class Generic extends Ast {
  private readonly _children: NodeChild[]

  constructor(span: Span, id?: AstId, children?: NodeChild[], treeType?: Tree.Type) {
    super(span, id, treeType)
    this._children = children ?? []
  }

  _rawChildren(): IterableIterator<NodeChild> {
    return this._children.values()
  }
}

export class NumericLiteral extends Ast {
  private readonly _tokens: NodeChild[]

  constructor(span: Span, id: AstId | undefined, tokens: NodeChild[]) {
    super(span, id, Tree.Type.Number)
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
  private _equals: NodeChild<Tok.Operator>
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
    span: Span,
    id: AstId | undefined,
    name: NodeChild<AstId>,
    args: FunctionArgument[],
    equals: NodeChild<Tok.Operator>, // Edits (#8367): NodeChild<Tok.Operator> | undefined
    body: NodeChild<AstId> | null,
  ) {
    super(span, id, Tree.Type.Function)
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
  private _equals: NodeChild<Tok.Operator>
  private _expression: NodeChild<AstId>
  get pattern(): Ast | null {
    return getNode(this._pattern.node)
  }
  get expression(): Ast | null {
    return getNode(this._expression.node)
  }
  constructor(
    span: Span,
    id: AstId | undefined,
    pattern: NodeChild<AstId>,
    equals: NodeChild<Tok.Operator>, // TODO: Edits (#8367): Allow undefined
    expression: NodeChild<AstId>,
  ) {
    super(span, id, Tree.Type.Assignment)
    this._pattern = pattern
    this._equals = equals
    this._expression = expression
  }
  // TODO: Edits (#8367)
  /*
  static new(
    id: AstId | undefined,
    ident: string,
    equals: WithWhitespace<Tok.Operator> | undefined,
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
  newline: NodeChild<Tok.Newline> // Edits (#8367): Allow undefined
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

  constructor(span: Span, id: AstId | undefined, lines: BlockLine[]) {
    super(span, id, Tree.Type.BodyBlock)
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
  public token: NodeChild<Tok.Ident>

  constructor(span: Span, id: AstId | undefined, token: NodeChild<Tok.Ident>) {
    super(span, id, Tree.Type.Ident)
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
  public token: NodeChild<Tok.Wildcard>

  constructor(span: Span, id: AstId | undefined, token: NodeChild<Tok.Wildcard>) {
    super(span, id, Tree.Type.Wildcard)
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

  constructor(span: Span, id: AstId | undefined, code: NodeChild) {
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
  const span = {
    start: codeStart,
    end: codeEnd,
    whitespaceLength: tree.whitespaceLengthInCodeParsed,
  }
  // All node types use this value in the same way to obtain the ID type, but each node does so separately because we
  // must pop the tree's span from the ID map *after* processing children.
  const spanKey = nodeKey(codeStart, codeEnd - codeStart, tree.type)
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
      const id = nodesExpected.get(spanKey)?.pop()
      node = new Block(span, id, lines)
      break
    }
    case Tree.Type.Function: {
      const name = abstract_(tree.name, code, nodesExpected, tokenIds)
      const args = Array.from(tree.args, (arg) => visitChildren(arg))
      const equals = abstractToken(tree.equals, code, tokenIds)
      const body =
        tree.body !== undefined ? abstract_(tree.body, code, nodesExpected, tokenIds) : null
      const id = nodesExpected.get(spanKey)?.pop()
      node = new Function(span, id, name, args, equals, body)
      break
    }
    case Tree.Type.Ident: {
      const id = nodesExpected.get(spanKey)?.pop()
      node = new Ident(span, id, abstractToken(tree.token, code, tokenIds))
      break
    }
    case Tree.Type.Assignment: {
      const pattern = abstract_(tree.pattern, code, nodesExpected, tokenIds)
      const equals = abstractToken(tree.equals, code, tokenIds)
      const value = abstract_(tree.expr, code, nodesExpected, tokenIds)
      const id = nodesExpected.get(spanKey)?.pop()
      node = new Assignment(span, id, pattern, equals, value)
      break
    }
    case Tree.Type.App: {
      const func = abstract_(tree.func, code, nodesExpected, tokenIds)
      const arg = abstract_(tree.arg, code, nodesExpected, tokenIds)
      const id = nodesExpected.get(spanKey)?.pop()
      node = positionalApp(span, id, func, arg)
      break
    }
    case Tree.Type.DefaultApp: {
      const func = abstract_(tree.func, code, nodesExpected, tokenIds)
      const token = abstractToken(tree.default, code, tokenIds)
      const arg = new Ident(token.node.span, undefined, token).exprId
      const id = nodesExpected.get(spanKey)?.pop()
      node = positionalApp(span, id, func, { whitespace: token.whitespace, node: arg })
      break
    }
    case Tree.Type.NamedApp: {
      const func = abstract_(tree.func, code, nodesExpected, tokenIds)
      const leftParen = tree.open ? abstractToken(tree.open, code, tokenIds) : null
      const name = abstractToken(tree.name, code, tokenIds)
      const equals = abstractToken(tree.equals, code, tokenIds)
      const arg = abstract_(tree.arg, code, nodesExpected, tokenIds)
      const rightParen = tree.close ? abstractToken(tree.close, code, tokenIds) : null
      const id = nodesExpected.get(spanKey)?.pop()
      node = namedApp(span, id, func, leftParen, name, equals, arg, rightParen)
      break
    }
    case Tree.Type.UnaryOprApp: {
      const opr = abstractToken(tree.opr, code, tokenIds)
      const arg = tree.rhs ? abstract_(tree.rhs, code, nodesExpected, tokenIds) : null
      const id = nodesExpected.get(spanKey)?.pop()
      if (opr.node.code() === '-') {
        node = new NegationOprApp(span, id, opr, arg)
      } else {
        node = new UnaryOprApp(span, id, opr, arg)
      }
      break
    }
    case Tree.Type.OprApp: {
      const lhs = tree.lhs ? abstract_(tree.lhs, code, nodesExpected, tokenIds) : null
      const opr = tree.opr.ok
        ? [abstractToken(tree.opr.value, code, tokenIds)]
        : visitChildren(tree.opr.error.payload)
      const rhs = tree.rhs ? abstract_(tree.rhs, code, nodesExpected, tokenIds) : null
      const id = nodesExpected.get(spanKey)?.pop()
      if (opr.length === 1 && opr[0]?.node instanceof Tok.Operator && opr[0].node.code() === '.') {
        // Propagate inferred type.
        const token = { whitespace: opr[0].whitespace, node: opr[0].node }
        node = new PropertyAccess(span, id, lhs, token, rhs)
      } else {
        node = new OprApp(span, id, lhs, opr, rhs)
      }
      break
    }
    case Tree.Type.Number: {
      const id = nodesExpected.get(spanKey)?.pop()
      node = new NumericLiteral(span, id, visitChildren(tree))
      break
    }
    case Tree.Type.Wildcard: {
      const token = abstractToken(tree.token, code, tokenIds)
      const id = nodesExpected.get(spanKey)?.pop()
      node = new Wildcard(span, id, token)
      break
    }
    default: {
      const id = nodesExpected.get(spanKey)?.pop()
      node = new Generic(span, id, visitChildren(tree), tree.type)
    }
  }
  const whitespace = code.substring(whitespaceStart, whitespaceEnd)
  return { node: node._id, whitespace }
}

function abstractToken(
  token: Token,
  code: string,
  tokenIds: TokenSpanMap,
): { whitespace: string; node: Tok.Tok } {
  const whitespaceStart = token.whitespaceStartInCodeBuffer
  const whitespaceEnd = whitespaceStart + token.whitespaceLengthInCodeBuffer
  const whitespace = code.substring(whitespaceStart, whitespaceEnd)
  const codeStart = token.startInCodeBuffer
  const codeEnd = codeStart + token.lengthInCodeBuffer
  const tokenCode = code.substring(codeStart, codeEnd)
  const span = { start: codeStart, end: codeEnd, whitespaceLength: whitespaceEnd - whitespaceStart }
  const key = tokenKey(codeStart, codeEnd - codeStart)
  const exprId = tokenIds.get(key) ?? newTokenId()
  const node = Tok.make(tokenCode, exprId, token.type, span)
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
    if (child.node instanceof Tok.Tok) {
      return child.node.code()
    } else {
      const node = (universe ?? edited).get(child.node)
      return node ? debug(node, universe) : '<missing>'
    }
  })
}

export function normalize(root: AstId): AstId {
  const printed = print(getNode(root)!)
  const tree = parseEnso(printed.code)
  return abstract(tree, printed.code, printed.info).node
}

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

export function parse(source: PrintedSource | string): Ast {
  return Ast.parse(source)
}

/** Parse using new AST types with old IdMap synchronization/editing. */
export function parseTransitional(code: string, idMap: IdMap): Ast {
  const legacyAst = AstExtended.parse(code, idMap)
  idMap.finishAndSynchronize()
  const nodes = new Map<NodeKey, AstId[]>()
  const tokens = new Map<TokenKey, TokenId>()
  const unique = new Set()
  legacyAst.visitRecursivePostorder((nodeOrToken: AstExtended<Tree | Token>) => {
    const start = nodeOrToken.span()[0]
    const length = nodeOrToken.span()[1] - nodeOrToken.span()[0]
    if (nodeOrToken.isToken()) {
      const token: AstExtended<Token> = nodeOrToken
      tokens.set(tokenKey(start, length), token.astId as TokenId)
    } else if (nodeOrToken.isTree()) {
      const node: AstExtended<Tree> = nodeOrToken
      const id = node.astId as AstId
      if (unique.has(id)) {
        // This shouldn't be happening, but it's probably due to a bug in `AstExtended`-related code that will be
        // replaced soon.
        console.warn(`Multiple occurrences of this UUID in tree:`, id)
      } else {
        unique.add(id)
        const key = nodeKey(start, length, node.inner.type)
        const ids = nodes.get(key)
        if (ids !== undefined) {
          ids.push(id)
        } else {
          nodes.set(key, [id])
        }
      }
    }
  })
  const newRoot = Ast.parse({ info: { nodes, tokens }, code })
  syncCommittedFromEdited()
  return newRoot
}

declare const AstKey: unique symbol
declare module '@/providers/widgetRegistry' {
  export interface WidgetInputTypes {
    [AstKey]: Ast
  }
}
