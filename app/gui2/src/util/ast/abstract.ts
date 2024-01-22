import { assertDefined, bail } from '@/util/assert'
import { parseEnso } from '@/util/ast'
import { unsafeEntries } from '@/util/record'
import { reactive } from 'vue'
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
  MutableModule,
  NegationOprApp,
  NumericLiteral,
  OprApp,
  PropertyAccess,
  ROOT_ID,
  TextLiteral,
  Token,
  UnaryOprApp,
  Wildcard,
  abstract,
  isTokenId,
  print,
  type AstFields,
  type AstId,
  type FixedMapView,
  type Module,
  type NodeKey,
  type Owned,
  type RootPointer,
  type SyncId,
  type SyncTokenId,
  type TokenId,
  type TokenKey,
} from '../../../shared/ast'
import { sourceRangeFromKey, type SourceRange } from '../../../shared/yjsModel'
export * from '../../../shared/ast'

export class ReactiveModule implements Module {
  edit(): MutableModule {
    return this.ymodule.edit()
  }

  root(): Ast | undefined {
    const rootPointer = this.getAst(ROOT_ID)
    if (!rootPointer) return
    const rootPointer_ = rootPointer as RootPointer
    return rootPointer_.expression
  }

  /////////////////////////////////

  private readonly ymodule: MutableModule
  private readonly nodes: Map<SyncId, FixedMapView<AstFields>>
  private readonly spans: Map<SyncId, SourceRange>

  constructor(base: MutableModule) {
    this.ymodule = base
    this.nodes = reactive(new Map())
    this.spans = reactive(new Map())
    base.observe((update) => {
      for (const id of update.addNodes) this.nodes.set(id, new Map() as any)
      for (const id of update.deleteNodes) this.nodes.delete(id)
      for (const { id, fields } of update.updateNodes) {
        const node = this.nodes.get(id)
        assertDefined(node)
        for (const [key, value] of fields) {
          const node_ = node as unknown as Map<string, unknown>
          node_.set(key, value)
        }
      }
      this.rebuildSpans(update.deleteNodes)
    })
  }

  private rebuildSpans(deleted: SyncId[]) {
    for (const id of deleted) this.spans.delete(id)
    const root = this.root()
    if (!root) return
    const printed = print(root)
    for (const [key, nodes] of printed.info.nodes) {
      const range = sourceRangeFromKey(key)
      for (const node of nodes) this.spans.set(node.fields.get('id'), range)
    }
  }

  getSpan(id: SyncId): SourceRange | undefined {
    return this.spans.get(id)
  }

  getAst(id: SyncId): Ast
  getAst(id: SyncId | undefined): Ast | undefined {
    if (!id) return
    const ast = this.tryGetAst(id)
    assertDefined(ast)
    return ast
  }

  tryGetAst(id: SyncId): Ast | undefined
  tryGetAst(id: SyncId | undefined): Ast | undefined {
    if (!id) return
    const fields = this.nodes.get(id)
    if (!fields) return
    return materialize(this, fields)
  }

  getToken(token: SyncTokenId): Token
  getToken(token: SyncTokenId | undefined): Token | undefined {
    if (!token) return token
    if (token instanceof Token) return token
    return new Token(token.code_, token.exprId, token.tokenType_)
  }

  getAny(node: SyncId | SyncTokenId): Ast | Token {
    return isTokenId(node) ? this.getToken(node) : this.getAst(node)
  }

  has(id: SyncId): boolean {
    return this.nodes.has(id)
  }
}

function materialize(module: Module, fields: FixedMapView<AstFields>): Ast {
  const type = fields.get('type')
  const fields_ = fields as FixedMapView<any>
  switch (type) {
    case 'App':
      return new App(module, fields_)
    case 'UnaryOprApp':
      return new UnaryOprApp(module, fields_)
    case 'NegationOprApp':
      return new NegationOprApp(module, fields_)
    case 'OprApp':
      return new OprApp(module, fields_)
    case 'PropertyAccess':
      return new PropertyAccess(module, fields_)
    case 'Generic':
      return new Generic(module, fields_)
    case 'Import':
      return new Import(module, fields_)
    case 'TextLiteral':
      return new TextLiteral(module, fields_)
    case 'Documented':
      return new Documented(module, fields_)
    case 'Invalid':
      return new Invalid(module, fields_)
    case 'Group':
      return new Group(module, fields_)
    case 'NumericLiteral':
      return new NumericLiteral(module, fields_)
    case 'Function':
      return new Function(module, fields_)
    case 'Assignment':
      return new Assignment(module, fields_)
    case 'BodyBlock':
      return new BodyBlock(module, fields_)
    case 'Ident':
      return new Ident(module, fields_)
    case 'Wildcard':
      return new Wildcard(module, fields_)
  }
  bail(`Invalid type: ${type}`)
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
      const node = module.tryGetAst(child.node)
      return node ? tokenTree(node) : '<missing>'
    }
  })
}

export function tokenTreeWithIds(root: Ast): TokenTree {
  const module = root.module
  return [
    root.exprId,
    ...Array.from(root.concreteChildren(), (child) => {
      if (isTokenId(child.node)) {
        return module.getToken(child.node).code()
      } else {
        const node = module.tryGetAst(child.node)
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

declare const TokenKey: unique symbol
declare module '@/providers/widgetRegistry' {
  export interface WidgetInputTypes {
    [TokenKey]: Token
  }
}
