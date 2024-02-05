import { assertDefined, bail } from '@/util/assert'
import { parseEnso } from '@/util/ast'
import { swapKeysAndValues, unsafeEntries } from '@/util/record'
import type {
  AstFields,
  AstId,
  FixedMapView,
  Module,
  ModuleUpdate,
  MutableAst,
  NodeKey,
  Owned,
  RootPointer,
  SyncTokenId,
  TokenId,
  TokenKey,
} from 'shared/ast'
import {
  Ast,
  BodyBlock,
  Function,
  MutableBodyBlock,
  MutableModule,
  ROOT_ID,
  Token,
  abstract,
  isTokenId,
  materialize,
  print,
} from 'shared/ast'
import { sourceRangeFromKey, type SourceRange } from 'shared/yjsModel'
import { reactive } from 'vue'
export * from 'shared/ast'

export class ReactiveModule implements Module {
  edit(): MutableModule {
    return this.ymodule.edit()
  }

  root(): Ast | undefined {
    const rootPointer = this.get(ROOT_ID)
    if (!rootPointer) return
    const rootPointer_ = rootPointer as RootPointer
    return rootPointer_.expression
  }

  /////////////////////////////////

  private readonly ymodule: MutableModule
  private readonly nodes: Map<AstId, FixedMapView<AstFields>> = reactive(new Map())
  private readonly spans: Map<AstId, SourceRange> = reactive(new Map())
  private readonly updateHooks: UpdateHandler[] = []

  constructor(base: MutableModule, updateHooks?: UpdateHandler[]) {
    this.ymodule = base
    this.updateHooks = [...(updateHooks ?? [])]
    // Attach the observer first, so that if an update hook causes changes in `base` in reaction to the initial state
    // update, we won't miss them.
    base.observe(this.handleUpdate.bind(this))
    this.handleUpdate(base.getStateAsUpdate())
  }

  disconnect() {
    this.updateHooks.length = 0
  }

  private handleUpdate(update: ModuleUpdate) {
    for (const id of update.nodesAdded) this.nodes.set(id, new Map() as any)
    for (const id of update.nodesDeleted) this.nodes.delete(id)
    for (const { id, fields } of update.fieldsUpdated) {
      const node = this.nodes.get(id)
      assertDefined(node)
      for (const [key, value] of fields) {
        const node_ = node as unknown as Map<string, unknown>
        node_.set(key, value)
      }
    }
    for (const { id, changes } of update.metadataUpdated) {
      const node = this.nodes.get(id)
      assertDefined(node)
      if (!node.has('metadata'))
        (node as unknown as Map<string, unknown>).set('metadata', new Map())
      const metadata = node.get('metadata') as unknown as Map<string, unknown>
      for (const [key, value] of changes) {
        if (value === undefined) {
          metadata.delete(key)
        } else {
          metadata.set(key, value)
        }
      }
    }
    this.rebuildSpans(update.nodesDeleted)
    const dirtyNodes = new Set<AstId>()
    for (const { id } of update.fieldsUpdated) dirtyNodes.add(id)
    for (const hook of this.updateHooks) hook(this, dirtyNodes, update.metadataUpdated)
  }

  private rebuildSpans(deleted: AstId[]) {
    for (const id of deleted) this.spans.delete(id)
    const root = this.root()
    if (!root) return
    const printed = print(root)
    for (const [key, nodes] of printed.info.nodes) {
      const range = sourceRangeFromKey(key)
      for (const node of nodes) this.spans.set(node.fields.get('id'), range)
    }
  }

  getSpan(id: AstId): SourceRange | undefined {
    return this.spans.get(id)
  }

  checkedGet(id: AstId): Ast
  checkedGet(id: AstId | undefined): Ast | undefined {
    if (!id) return
    const ast = this.get(id)
    assertDefined(ast)
    return ast
  }

  get(id: AstId): Ast | undefined
  get(id: AstId | undefined): Ast | undefined {
    if (!id) return
    const fields = this.nodes.get(id)
    if (!fields) return
    return materialize(this, fields)
  }

  getToken(token: SyncTokenId): Token
  getToken(token: SyncTokenId | undefined): Token | undefined {
    if (!token) return token
    if (token instanceof Token) return token
    return Token.withId(token.code_, token.tokenType_, token.id)
  }

  getAny(node: AstId | SyncTokenId): Ast | Token {
    return isTokenId(node) ? this.getToken(node) : this.checkedGet(node)
  }

  has(id: AstId): boolean {
    return this.nodes.has(id)
  }
}

export interface SetView<Key> {
  readonly size: number
  has(key: Key): boolean
  [Symbol.iterator](): IterableIterator<Key>
}

type UpdateHandler = (
  module: ReactiveModule,
  dirtyNodes: SetView<AstId>,
  metadataChanges: { id: AstId; changes: Map<string, unknown> }[],
) => void

export class EmptyModule implements Module {
  edit(): never {
    bail(`EmptyModule cannot be edited.`)
  }
  root(): undefined {
    return
  }
  get(_id: AstId | undefined): undefined {
    return
  }
  checkedGet(id: AstId): never
  checkedGet(id: AstId | undefined): undefined {
    if (id) bail(`${id} is not in an EmptyModule.`)
  }
  getToken(token: SyncTokenId): never
  getToken(token: SyncTokenId | undefined): undefined {
    if (token) bail(`EmptyModule contains no tokens.`)
    return
  }
  getAny(node: AstId | SyncTokenId): never {
    bail(`EmptyModule does not contain ${node}.`)
  }
  has(_id: AstId): false {
    return false
  }
  getSpan(_id: AstId): undefined {
    return
  }
}

const mapping: Record<string, string> = {
  '\b': '\\b',
  '\f': '\\f',
  '\n': '\\n',
  '\r': '\\r',
  '\t': '\\t',
  '\v': '\\v',
  '"': '\\"',
  "'": "\\'",
  '`': '``',
}

const reverseMapping = swapKeysAndValues(mapping)

/** Escape a string so it can be safely spliced into an interpolated (`''`) Enso string.
 * NOT USABLE to insert into raw strings. Does not include quotes. */
export function escape(string: string) {
  return string.replace(/[\0\b\f\n\r\t\v"'`]/g, (match) => mapping[match]!)
}

/** The reverse of `escape`: transform the string into human-readable form, not suitable for interpolation. */
export function unescape(string: string) {
  return string.replace(/\\[0bfnrtv"']|``/g, (match) => reverseMapping[match]!)
}

export function deserialize(serialized: string): Owned {
  const parsed: SerializedPrintedSource = JSON.parse(serialized)
  const nodes = new Map(unsafeEntries(parsed.info.nodes))
  const tokens = new Map(unsafeEntries(parsed.info.tokens))
  const module = MutableModule.Transient()
  const tree = parseEnso(parsed.code)
  const ast = abstract(module, tree, parsed.code)
  // TODO: ast <- nodes,tokens
  return ast.root
}

interface SerializedInfoMap {
  nodes: Record<NodeKey, AstId[]>
  tokens: Record<TokenKey, TokenId>
}

interface SerializedPrintedSource {
  info: SerializedInfoMap
  code: string
}

export function serialize(ast: Ast): string {
  return JSON.stringify(print(ast))
}

export type TokenTree = (TokenTree | string)[]
export function tokenTree(root: Ast): TokenTree {
  const module = root.module
  return Array.from(root.concreteChildren(), (child) => {
    if (isTokenId(child.node)) {
      return module.getToken(child.node).code()
    } else {
      const node = module.get(child.node)
      return node ? tokenTree(node) : '<missing>'
    }
  })
}

export function tokenTreeWithIds(root: Ast): TokenTree {
  const module = root.module
  return [
    root.externalId,
    ...Array.from(root.concreteChildren(), (child) => {
      if (isTokenId(child.node)) {
        return module.getToken(child.node).code()
      } else {
        const node = module.get(child.node)
        return node ? tokenTreeWithIds(node) : ['<missing>']
      }
    }),
  ]
}

export function moduleMethodNames(topLevel: BodyBlock): Set<string> {
  const result = new Set<string>()
  for (const statement of topLevel.statements()) {
    const inner = statement.innerExpression()
    if (inner instanceof Function) {
      result.add(inner.name.code())
    }
  }
  return result
}

// FIXME: We should use alias analysis to handle ambiguous names correctly.
export function findModuleMethod(topLevel: BodyBlock, name: string): Function | undefined {
  for (const statement of topLevel.statements()) {
    const inner = statement.innerExpression()
    if (inner instanceof Function && inner.name.code() === name) {
      return inner
    }
  }
  return undefined
}

export function functionBlock(topLevel: BodyBlock, name: string) {
  const func = findModuleMethod(topLevel, name)
  if (!(func?.body instanceof BodyBlock)) return undefined
  return func.body
}

export function deleteFromParentBlock(ast: MutableAst) {
  const parent = ast.mutableParent()
  if (parent instanceof MutableBodyBlock)
    parent.updateLines((lines) => lines.filter((line) => line.expression?.node.id !== ast.id))
}

declare const tokenKey: unique symbol
declare module '@/providers/widgetRegistry' {
  export interface WidgetInputTypes {
    [tokenKey]: Token
  }
}
