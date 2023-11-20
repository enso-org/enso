import * as Ast from '@/generated/ast'
import { Token, Tree } from '@/generated/ast'
import { assert } from '@/util/assert'
import * as encoding from 'lib0/encoding'
import { digest } from 'lib0/hash/sha256'
import * as map from 'lib0/map'

import type { ContentRange, ExprId, IdMap } from 'shared/yjsModel'
import { markRaw } from 'vue'
import {
  childrenAstNodesOrTokens,
  debugAst,
  parseEnso,
  parsedTreeOrTokenRange,
  readAstOrTokenSpan,
  visitGenerator,
  visitRecursive,
  walkRecursive,
} from '.'
import type { Opt } from '../opt'

/**
 * AST with additional metadata containing AST IDs and original code reference. Can only be
 * constructed by parsing any enso source code string.
 */
export class AstExtended<T extends Tree | Token = Tree | Token, HasIdMap extends boolean = true> {
  inner: T
  private ctx: AstExtendedCtx<HasIdMap>

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

  treeTypeName(): (typeof Tree.typeNames)[number] | null {
    return Tree.isInstance(this.inner) ? Tree.typeNames[this.inner.type] : null
  }

  tokenTypeName(): (typeof Token.typeNames)[number] | null {
    return Token.isInstance(this.inner) ? Token.typeNames[this.inner.type] : null
  }

  isToken<T extends Ast.Token.Type>(
    type?: T,
  ): this is AstExtended<Extract<Ast.Token, { type: T }>, HasIdMap> {
    return Token.isInstance(this.inner) && (type == null || this.inner.type === type)
  }

  isTree<T extends Ast.Tree.Type>(
    type?: T,
  ): this is AstExtended<Extract<Ast.Tree, { type: T }>, HasIdMap> {
    return Tree.isInstance(this.inner) && (type == null || this.inner.type === type)
  }

  private constructor(tree: T, ctx: AstExtendedCtx<HasIdMap>) {
    markRaw(this)
    this.inner = tree
    this.ctx = ctx
  }

  get astId(): CondType<ExprId, HasIdMap> {
    if (this.ctx.idMap != null) {
      const id = this.ctx.idMap.getIfExist(parsedTreeOrTokenRange(this.inner))
      assert(id != null, 'All AST nodes should have an assigned ID')
      return id as CondType<ExprId, HasIdMap>
    } else {
      return undefined as CondType<ExprId, HasIdMap>
    }
  }

  debug(): unknown {
    return debugAst(this.inner)
  }

  tryMap<T2 extends Tree>(mapper: (t: T) => Opt<T2>): AstExtended<T2, HasIdMap> | undefined {
    const mapped = mapper(this.inner)
    if (mapped == null) return
    return new AstExtended(mapped, this.ctx)
  }

  map<T2 extends Tree | Token>(mapper: (t: T) => T2): AstExtended<T2, HasIdMap> {
    return new AstExtended(mapper(this.inner), this.ctx)
  }

  repr() {
    return readAstOrTokenSpan(this.inner, this.ctx.parsedCode)
  }

  span(): ContentRange {
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

  visitRecursive(visitor: (t: AstExtended<Tree | Token, HasIdMap>) => boolean) {
    visitGenerator(this.walkRecursive(), visitor)
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
      digest(
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
