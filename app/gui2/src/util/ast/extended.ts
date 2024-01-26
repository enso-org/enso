import * as Ast from '@/generated/ast'
import { Token, Tree } from '@/generated/ast'
import { assert } from '@/util/assert'
import {
  childrenAstNodesOrTokens,
  parseEnso,
  parsedTreeOrTokenRange,
  readAstOrTokenSpan,
  visitGenerator,
  visitRecursive,
  walkRecursive,
} from '@/util/ast'
import type { Opt } from '@/util/data/opt'
import * as encoding from 'lib0/encoding'
import * as sha256 from 'lib0/hash/sha256'
import * as map from 'lib0/map'
import type { ExternalId, IdMap, SourceRange } from 'shared/yjsModel'
import { markRaw } from 'vue'

type ExtractType<V, T> = T extends ReadonlyArray<infer Ts>
  ? Extract<V, { type: Ts }>
  : Extract<V, { type: T }>

type OneOrArray<T> = T | readonly T[]

/**
 * AST with additional metadata containing AST IDs and original code reference. Can only be
 * constructed by parsing any enso source code string.
 */
export class AstExtended<T extends Tree | Token = Tree | Token, HasIdMap extends boolean = true> {
  inner: T
  private ctx: AstExtendedCtx<HasIdMap>

  public static isToken<T extends OneOrArray<Ast.Token.Type>>(type?: T) {
    return (obj: unknown): obj is AstExtended<ExtractType<Ast.Token, T>, boolean> =>
      obj instanceof AstExtended && obj.isToken(type)
  }

  public static isTree<T extends OneOrArray<Ast.Tree.Type>>(type?: T) {
    return (obj: unknown): obj is AstExtended<ExtractType<Ast.Tree, T>, boolean> =>
      obj instanceof AstExtended && obj.isTree(type)
  }

  public static parse(code: string): AstExtended<Tree, false>
  public static parse(code: string, idMap: IdMap): AstExtended<Tree, true>
  public static parse(code: string, idMap?: IdMap): AstExtended<Tree, boolean> {
    const ast = parseEnso(code)
    if (idMap != null) {
      visitRecursive(ast, (node) => {
        const range = parsedTreeOrTokenRange(node)
        idMap.getOrInsertUniqueId(range)
        return true
      })
    }

    const ctx = new AstExtendedCtx(code, idMap)
    return new AstExtended(ast, ctx)
  }

  public static parseLine(code: string): AstExtended<Tree, false> {
    const block = AstExtended.parse(code)
    assert(block.isTree(Tree.Type.BodyBlock))
    return block.map((block) => {
      const statements = block.statements[Symbol.iterator]()
      const firstLine = statements.next()
      assert(!firstLine.done)
      assert(!!statements.next().done)
      assert(firstLine.value.expression != null)
      return firstLine.value.expression
    })
  }

  isToken<T extends OneOrArray<Ast.Token.Type>>(
    type?: T,
  ): this is AstExtended<ExtractType<Ast.Token, T>, HasIdMap> {
    if (!Token.isInstance(this.inner)) return false
    if (type == null) return true
    if (Array.isArray(type)) return (type as Ast.Token.Type[]).includes(this.inner.type)
    return this.inner.type === type
  }

  isTree<T extends OneOrArray<Ast.Tree.Type>>(
    type?: T,
  ): this is AstExtended<ExtractType<Ast.Tree, T>, HasIdMap> {
    if (!Tree.isInstance(this.inner)) return false
    if (type == null) return true
    if (Array.isArray(type)) return (type as Ast.Tree.Type[]).includes(this.inner.type)
    return this.inner.type === type
  }

  private constructor(tree: T, ctx: AstExtendedCtx<HasIdMap>) {
    markRaw(this)
    this.inner = tree
    this.ctx = ctx
  }

  get astId(): CondType<ExternalId, HasIdMap> {
    if (this.ctx.idMap != null) {
      const id = this.ctx.idMap.getIfExist(parsedTreeOrTokenRange(this.inner))
      assert(id != null, 'All AST nodes should have an assigned ID')
      return id as CondType<ExternalId, HasIdMap>
    } else {
      return undefined as CondType<ExternalId, HasIdMap>
    }
  }

  tryMap<T2 extends Tree | Token>(
    mapper: (t: T) => Opt<T2>,
  ): AstExtended<T2, HasIdMap> | undefined {
    const mapped = mapper(this.inner)
    if (mapped == null) return
    return new AstExtended(mapped, this.ctx)
  }

  map<T2 extends Tree | Token>(mapper: (t: T) => T2): AstExtended<T2, HasIdMap> {
    return new AstExtended(mapper(this.inner), this.ctx)
  }

  mapIter<T2 extends Tree | Token>(
    mapper: (t: T) => Iterable<T2>,
  ): Iterable<AstExtended<T2, HasIdMap>> {
    return [...mapper(this.inner)].map((m) => new AstExtended(m, this.ctx))
  }

  tryMapIter<T2 extends Tree | Token>(
    mapper: (t: T) => Iterable<Opt<T2>>,
  ): Iterable<AstExtended<T2, HasIdMap> | undefined> {
    return [...mapper(this.inner)].map((m) =>
      m != null ? new AstExtended(m, this.ctx) : undefined,
    )
  }

  repr() {
    return readAstOrTokenSpan(this.inner, this.ctx.parsedCode)
  }

  span(): SourceRange {
    return parsedTreeOrTokenRange(this.inner)
  }

  contentHash() {
    return this.ctx.getHash(this)
  }

  children(): AstExtended<Tree | Token, HasIdMap>[] {
    return childrenAstNodesOrTokens(this.inner).map((child) => new AstExtended(child, this.ctx))
  }

  walkRecursive(): Generator<AstExtended<Tree | Token, HasIdMap>> {
    return this.visit(walkRecursive)
  }

  whitespaceLength() {
    return 'whitespaceLengthInCodeBuffer' in this.inner
      ? this.inner.whitespaceLengthInCodeBuffer
      : this.inner.whitespaceLengthInCodeParsed
  }

  *visit<T2 extends Tree | Token>(
    visitor: (t: T) => Generator<T2>,
  ): Generator<AstExtended<T2, HasIdMap>> {
    for (const child of visitor(this.inner)) {
      yield new AstExtended(child, this.ctx)
    }
  }

  /**
   * Recursively visit AST nodes in depth-first order. The children of a node will be skipped when
   * `visit` callback returns `false`.
   *
   * @param node Root node of the tree to walk. It will be visited first.
   * @param visit Callback that is called for each node. If it returns `false`, the children of that
   * node will be skipped, and the walk will continue to the next sibling.
   */
  visitRecursive(visitor: (t: AstExtended<Tree | Token, HasIdMap>) => boolean) {
    visitGenerator(this.walkRecursive(), visitor)
  }

  get parsedCode() {
    return this.ctx.parsedCode
  }
}

type CondType<T, Cond extends boolean> = Cond extends true
  ? T
  : Cond extends false
  ? undefined
  : T | undefined

class AstExtendedCtx<HasIdMap extends boolean> {
  parsedCode: string
  idMap: CondType<IdMap, HasIdMap>
  contentHashes: Map<string, Uint8Array>

  constructor(parsedCode: string, idMap: CondType<IdMap, HasIdMap>) {
    this.parsedCode = parsedCode
    this.idMap = idMap
    this.contentHashes = new Map()
  }

  static getHashKey(ast: AstExtended<Tree | Token, boolean>) {
    return `${ast.isToken() ? 'T.' : ''}${ast.inner.type}.${ast.span()[0]}`
  }

  getHash(ast: AstExtended<Tree | Token, boolean>) {
    const key = AstExtendedCtx.getHashKey(ast)
    return map.setIfUndefined(this.contentHashes, key, () =>
      sha256.digest(
        encoding.encode((encoder) => {
          const whitespace = ast.whitespaceLength()
          encoding.writeUint32(encoder, whitespace)
          if (ast.isToken()) {
            encoding.writeUint8(encoder, 0)
            encoding.writeUint32(encoder, ast.inner.type)
            encoding.writeVarString(encoder, ast.repr())
          } else {
            encoding.writeUint8(encoder, 1)
            encoding.writeUint32(encoder, ast.inner.type)
            for (const child of ast.children()) {
              encoding.writeUint8Array(encoder, this.getHash(child))
            }
          }
        }),
      ),
    )
  }
}
