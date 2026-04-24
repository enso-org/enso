import {
  SourceRange,
  sourceRangeFromKey,
  sourceRangeKey,
  type SourceRangeKey,
} from '../util/data/text'
import { IdMap, type ExternalId } from '../yjsModel'
import type { Token, TokenId } from './token'
import { ExpressionStatement, type Ast, type AstId } from './tree'

declare const nodeKeyBrand: unique symbol
/** A source-range key for an `Ast`. */
export type NodeKey = SourceRangeKey & { [nodeKeyBrand]: never }
declare const tokenKeyBrand: unique symbol
/** A source-range key for a `Token`. */
export type TokenKey = SourceRangeKey & { [tokenKeyBrand]: never }
/** Create a source-range key for an `Ast`. */
export function nodeKey(start: number, length: number): NodeKey {
  return sourceRangeKey(SourceRange.fromStartAndLength(start, length)) as NodeKey
}
/** Create a source-range key for a `Token`. */
export function tokenKey(start: number, length: number): TokenKey {
  return sourceRangeKey(SourceRange.fromStartAndLength(start, length)) as TokenKey
}

/** Maps from source ranges to `Ast`s. */
export type NodeSpanMap = Map<NodeKey, Ast[]>
/** Maps from source ranges to `Token`s. */
export type TokenSpanMap = Map<TokenKey, Token>

/** Maps from source ranges to `Ast`s and `Token`s. */
export interface SpanMap {
  nodes: NodeSpanMap
  tokens: TokenSpanMap
}

/** Create a new random {@link ExternalId}. */
export function newExternalId(): ExternalId {
  return crypto.randomUUID() as ExternalId
}

/** Generate an `IdMap` from a `SpanMap`. */
export function spanMapToIdMap(spans: SpanMap): IdMap {
  const idMap = new IdMap()
  for (const [key, token] of spans.tokens.entries()) {
    idMap.insertKnownId(sourceRangeFromKey(key), token.id)
  }
  for (const [key, asts] of spans.nodes.entries()) {
    for (const ast of asts) {
      if (ast instanceof ExpressionStatement && asts.length > 1) continue
      idMap.insertKnownId(sourceRangeFromKey(key), ast.externalId)
    }
  }
  return idMap
}

/** Returns a function that can look up source ranges by AST ID. */
export function spanMapToSpanGetter(spans: NodeSpanMap): (id: AstId) => SourceRange | undefined {
  const reverseMap = new Map<AstId, SourceRange>()
  for (const [key, asts] of spans) {
    for (const ast of asts) {
      reverseMap.set(ast.id, sourceRangeFromKey(key))
    }
  }
  return (id) => reverseMap.get(id)
}

/** Returns a function that can look up token source ranges. */
export function tokenSpanGetter(spans: TokenSpanMap): (token: Token) => SourceRange | undefined {
  const reverseMap = new Map<TokenId, SourceRange>()
  for (const [key, token] of spans) reverseMap.set(token.id, sourceRangeFromKey(key))
  return ({ id }) => reverseMap.get(id)
}
