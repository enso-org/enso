import { Token, Tree } from '@/generated/ast'
import { parseEnso } from '@/util/ast'
import type { LazyObject } from '@/util/parserSupport'
import * as random from 'lib0/random'
import { reactive } from 'vue'
import type { ExprId } from '../../../shared/yjsModel'

const committed = reactive(new Map<AstId, Ast>())
/** New nodes, COW-copies of modified nodes, and pending deletions (nulls) */
const edited = new Map<AstId, Ast | null>()

/** Returns a syntax node representing the current committed state of the given ID. */
function getNode(id: AstId): Ast | null {
  return committed.get(id) ?? null
}

/** Returns a syntax node representing the current state of the given ID, including any uncommitted modifications. */
function getUncommitted(id: AstId): Ast | null {
  if (edited.has(id)) {
    return edited.get(id) ?? null
  } else {
    return committed.get(id) ?? null
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

export type NodeChild = { whitespace?: string | undefined; node: AstId | Tok }
type AstWithWhitespace = { whitespace?: string | undefined; node: AstId }
type TokWithWhitespace = { whitespace?: string | undefined; node: Tok }

function token(code: string, type: Token.Type): Tok {
  return new Tok(code, newTokenId(), type)
}

export function isToken(node: Tok | AstId): node is Tok {
  return node instanceof Tok
}

export abstract class Expression {
  abstract code(): string

  /** Returns child subtrees, without information about the whitespace between them. */
  *children(): Iterable<Ast | Tok> {
    for (const child of this._rawChildren()) {
      if (child.node instanceof Tok) {
        yield child.node
      } else {
        const node = getNode(child.node)
        if (node) yield node
      }
    }
  }

  /** Returns child concrete subtrees. */
  abstract _rawChildren(): Iterable<NodeChild>
}

export class Tok extends Expression {
  private _code: string
  exprId: TokenId
  readonly _tokenType: Token.Type
  constructor(code: string, id: TokenId, type: Token.Type) {
    super()
    this._code = code
    this.exprId = id
    this._tokenType = type
  }

  code(): string {
    return this._code
  }

  _rawChildren(): Iterable<NodeChild> {
    return []
  }

  typeName(): string {
    return Token.typeNames[this._tokenType]!
  }
}

type Span = { start: number; end: number; whitespaceLength: number }
export abstract class Ast extends Expression {
  readonly treeType: Tree.Type | undefined
  _id: AstId
  // TODO: Eliminate this before enabling edit support.
  readonly span: Span

  get exprId(): AstId {
    return this._id
  }

  code(): string {
    return this._print().code
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
      if (isToken(child.node)) {
        visit(child.node)
      } else {
        getNode(child.node)?.visitRecursive(visit)
      }
    }
  }

  protected constructor(span: Span, id?: AstId, treeType?: Tree.Type) {
    super()
    this._id = id ?? newNodeId()
    this.treeType = treeType
    this.span = span
    edited.set(this._id, this)
  }

  _print(): PrintedSource {
    const info: InfoMap = {
      nodes: new Map(),
      tokens: new Map(),
    }
    const code = this.__print(info, 0, '')
    return { info, code }
  }

  __print(info: InfoMap, offset: number, indent: string): string {
    let code = ''
    for (const child of this._rawChildren()) {
      if (child.node != null && !isToken(child.node) && getUncommitted(child.node) === null)
        continue
      if (child.whitespace != null) {
        code += child.whitespace
      } else if (code.length != 0) {
        // TODO: Identify cases where a space should not be inserted.
        code += ' '
      }
      if (child.node != null) {
        if (isToken(child.node)) {
          code += child.node.code()
        } else {
          code += getUncommitted(child.node)!.__print(info, offset + code.length, indent)
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

  // TODO: Editing.
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
  private _func: AstWithWhitespace
  private _leftParen: TokWithWhitespace | null
  private _name: TokWithWhitespace | null
  private _equals: TokWithWhitespace | null
  private _arg: NodeChild
  private _rightParen: TokWithWhitespace | null

  constructor(
    span: Span,
    id: AstId | undefined,
    func: AstWithWhitespace,
    leftParen: TokWithWhitespace | null,
    name: TokWithWhitespace | null,
    equals: TokWithWhitespace | null,
    arg: NodeChild,
    rightParen: TokWithWhitespace | null,
    treeType: Tree.Type,
  ) {
    super(span, id, treeType)
    this._func = func
    this._leftParen = leftParen
    this._name = name
    this._equals = equals
    this._arg = arg
    this._rightParen = rightParen
  }

  static _positional(
    span: Span,
    id: AstId | undefined,
    func: AstWithWhitespace,
    arg: NodeChild,
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
      isToken(arg.node) ? Tree.Type.DefaultApp : Tree.Type.App,
    )
  }

  static _named(
    span: Span,
    id: AstId | undefined,
    func: AstWithWhitespace,
    leftParen: TokWithWhitespace | null,
    name: TokWithWhitespace,
    equals: TokWithWhitespace | undefined,
    arg: NodeChild,
    rightParen: TokWithWhitespace | null,
  ) {
    return new App(
      span,
      id,
      func,
      leftParen,
      name,
      equals ?? { node: token('=', Token.Type.Operator) },
      arg,
      rightParen,
      Tree.Type.NamedApp,
    )
  }

  *_rawChildren(): Iterable<NodeChild> {
    yield this._func
    if (this._leftParen) yield this._leftParen
    if (this._name) yield this._name
    if (this._equals)
      yield { whitespace: this._equals.whitespace ?? this._arg.whitespace, node: this._equals.node }
    yield this._arg
    if (this._rightParen) yield this._rightParen
  }
}

export class UnaryOprApp extends Ast {
  private _opr: TokWithWhitespace
  private _arg: AstWithWhitespace | null

  get operator(): Tok {
    return this._opr.node
  }

  get argument(): Ast | null {
    const id = this._arg?.node
    return id ? getNode(id) : null
  }

  constructor(
    span: Span,
    id: AstId | undefined,
    opr: TokWithWhitespace,
    arg: AstWithWhitespace | null,
  ) {
    super(span, id, Tree.Type.UnaryOprApp)
    this._opr = opr
    this._arg = arg
  }

  *_rawChildren(): Iterable<NodeChild> {
    yield this._opr
    if (this._arg) yield this._arg
  }
}

export class NegationOprApp extends UnaryOprApp {
  constructor(
    span: Span,
    id: AstId | undefined,
    opr: TokWithWhitespace,
    arg: AstWithWhitespace | null,
  ) {
    super(span, id, opr, arg)
  }
}

export class OprApp extends Ast {
  protected _lhs: AstWithWhitespace | null
  protected _opr: NodeChild[]
  protected _rhs: AstWithWhitespace | null

  get lhs(): Ast | null {
    return this._lhs ? getNode(this._lhs.node) : null
  }

  get rhs(): Ast | null {
    return this._rhs ? getNode(this._rhs.node) : null
  }

  constructor(
    span: Span,
    id: AstId | undefined,
    lhs: AstWithWhitespace | null,
    opr: NodeChild[],
    rhs: AstWithWhitespace | null,
  ) {
    super(span, id, Tree.Type.OprApp)
    this._lhs = lhs
    this._opr = opr
    this._rhs = rhs
  }

  *_rawChildren(): Iterable<NodeChild> {
    if (this._lhs) yield this._lhs
    for (const opr of this._opr) yield opr
    if (this._rhs) yield this._rhs
  }
}

export class PropertyAccess extends OprApp {
  constructor(
    span: Span,
    id: AstId | undefined,
    lhs: AstWithWhitespace | null,
    opr: TokWithWhitespace,
    rhs: AstWithWhitespace | null,
  ) {
    super(span, id, lhs, [opr], rhs)
  }
}

export class Generic extends Ast {
  private readonly _children: NodeChild[]

  constructor(span: Span, id?: AstId, children?: NodeChild[], treeType?: Tree.Type) {
    super(span, id, treeType)
    this._children = children ?? []
  }

  _rawChildren(): Iterable<NodeChild> {
    return this._children
  }
}

export class NumericLiteral extends Ast {
  private readonly _tokens: NodeChild[]

  constructor(span: Span, id: AstId | undefined, tokens: NodeChild[]) {
    super(span, id, Tree.Type.Number)
    this._tokens = tokens ?? []
  }

  _rawChildren(): Iterable<NodeChild> {
    return this._tokens
  }
}

type FunctionArgument = NodeChild[]
export class Function extends Ast {
  private _name: AstWithWhitespace
  private _args: FunctionArgument[]
  private _equals: TokWithWhitespace | undefined
  private _body: AstWithWhitespace | null
  // FIXME: This should not be nullable. If the `ExprId` has been deleted, the same placeholder logic should be applied
  //  here and in `rawChildren` (and indirectly, `print`).
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
    span: Span,
    id: AstId | undefined,
    name: AstWithWhitespace,
    args: FunctionArgument[],
    equals: TokWithWhitespace | undefined,
    body: AstWithWhitespace | null,
  ) {
    super(span, id, Tree.Type.Function)
    this._name = name
    this._args = args
    this._equals = equals
    this._body = body
  }
  *_rawChildren(): Iterable<NodeChild> {
    yield this._name
    for (const arg of this._args) yield* arg
    if (this._equals !== undefined) {
      yield { whitespace: this._equals.whitespace ?? ' ', node: this._equals.node }
    } else {
      yield { whitespace: ' ', node: token('=', Token.Type.Operator) }
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
    span: Span,
    id: AstId | undefined,
    pattern: AstWithWhitespace,
    equals: TokWithWhitespace | undefined,
    expression: AstWithWhitespace,
  ) {
    super(span, id, Tree.Type.Assignment)
    this._pattern = pattern
    this._equals = equals
    this._expression = expression
  }
  // TODO: Edits
  /*
  static new(
    id: AstId | undefined,
    ident: string,
    equals: TokWithWhitespace | undefined,
    expression: AstWithWhitespace,
  ): Assignment {
    const pattern = { node: Ident.new(ident)._id }
    return new Assignment(id, pattern, equals, expression)
  }
   */
  *_rawChildren(): Iterable<NodeChild> {
    yield this._pattern
    if (this._equals !== undefined) {
      yield { whitespace: this._equals.whitespace ?? ' ', node: this._equals.node }
    } else {
      yield { whitespace: ' ', node: token('=', Token.Type.Operator) }
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

  constructor(span: Span, id: AstId | undefined, lines: BlockLine[]) {
    super(span, id, Tree.Type.BodyBlock)
    this._lines = lines
  }

  // TODO: Edits
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

  *_rawChildren(): Iterable<NodeChild> {
    for (const line of this._lines) {
      yield line.newline ?? { node: token('\n', Token.Type.Newline) }
      if (line.expression !== null) yield line.expression
    }
  }

  __print(info: InfoMap, offset: number, indent: string): string {
    let code = ''
    for (const line of this._lines) {
      if (
        line.expression?.node != null &&
        !isToken(line.expression.node) &&
        getUncommitted(line.expression.node) === null
      )
        continue
      code += line.newline?.whitespace ?? ''
      code += line.newline?.node.code() ?? '\n'
      if (line.expression !== null) {
        code += line.expression.whitespace ?? indent
        if (line.expression.node != null) {
          if (isToken(line.expression.node)) {
            code += line.expression.node.code
          } else {
            code += getUncommitted(line.expression.node)!.__print(info, offset, indent + '    ')
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

  constructor(span: Span, id: AstId | undefined, token: TokWithWhitespace) {
    super(span, id, Tree.Type.Ident)
    this.token = token
  }

  static _fromToken(span: Span, id: AstId | undefined, token: TokWithWhitespace): Ident {
    return new Ident(span, id, token)
  }

  // TODO: Edits
  /*
  static new(id: AstId | undefined, code: string): Ident {
    return Ident.fromToken(id, { node: token(code, Token.Type.Ident) })
  }
   */

  *_rawChildren(): Iterable<NodeChild> {
    yield this.token
  }
}

export class Placeholder extends Ast {
  public token: TokWithWhitespace

  constructor(span: Span, id: AstId | undefined, token: TokWithWhitespace) {
    super(span, id, Tree.Type.Wildcard)
    this.token = token
  }

  *_rawChildren(): Iterable<NodeChild> {
    yield this.token
  }
}

// TODO: Edits
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

  *_rawChildren(): Iterable<NodeChild> {
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
      node = Ident._fromToken(span, id, abstractToken(tree.token, code, tokenIds))
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
      node = App._positional(span, id, func, arg)
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
      node = App._named(span, id, func, leftParen, name, equals, arg, rightParen)
      break
    }
    case Tree.Type.DefaultApp: {
      const func = abstract_(tree.func, code, nodesExpected, tokenIds)
      const arg = abstractToken(tree.default, code, tokenIds)
      const id = nodesExpected.get(spanKey)?.pop()
      node = App._positional(span, id, func, arg)
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
      if (opr.length === 1 && opr[0]?.node instanceof Tok && opr[0].node.code() === '.') {
        // Propogate inferred type.
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
      node = new Placeholder(span, id, token)
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
): { whitespace: string; node: Tok } {
  const whitespaceStart = token.whitespaceStartInCodeBuffer
  const whitespaceEnd = whitespaceStart + token.whitespaceLengthInCodeBuffer
  const whitespace = code.substring(whitespaceStart, whitespaceEnd)
  const codeStart = token.startInCodeBuffer
  const codeEnd = codeStart + token.lengthInCodeBuffer
  const tokenCode = code.substring(codeStart, codeEnd)
  const span = tokenKey(codeStart, codeEnd - codeStart, token.type)
  const exprId = tokenIds.get(span) ?? newTokenId()
  const node = new Tok(tokenCode, exprId, token.type)
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
export function debug(root: Ast): DebugTree {
  return Array.from(root._rawChildren(), (child) => {
    if (isToken(child.node)) {
      return child.node.code()
    } else {
      const node = getUncommitted(child.node)
      return node ? debug(node) : '<missing>'
    }
  })
}

export function normalize(root: AstId): AstId {
  const printed = getNode(root)!._print()
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

// TODO: Use new Ast for edits.
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
