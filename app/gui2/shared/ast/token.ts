import type { AstId, Owned } from '.'
import { Ast, newExternalId } from '.'
import { assert } from '../util/assert'
import type { ExternalId } from '../yjsModel'
import { isUuid } from '../yjsModel'
import { is_ident_or_operator } from './ffi'
import * as RawAst from './generated/ast'

export function isToken(t: unknown): t is Token {
  return t instanceof Token
}

declare const brandTokenId: unique symbol
export type TokenId = ExternalId & { [brandTokenId]: never }

function newTokenId(): TokenId {
  return newExternalId() as TokenId
}

/** @internal */
export interface SyncTokenId {
  readonly id: TokenId
  code_: string
  tokenType_: RawAst.Token.Type | undefined
}
export class Token implements SyncTokenId {
  readonly id: TokenId
  code_: string
  tokenType_: RawAst.Token.Type | undefined

  private constructor(code: string, type: RawAst.Token.Type | undefined, id: TokenId) {
    this.id = id
    this.code_ = code
    this.tokenType_ = type
  }

  get externalId(): TokenId {
    return this.id
  }

  static new(code: string, type?: RawAst.Token.Type) {
    return new this(code, type, newTokenId())
  }

  static withId(code: string, type: RawAst.Token.Type | undefined, id: TokenId) {
    assert(isUuid(id))
    return new this(code, type, id)
  }

  code(): string {
    return this.code_
  }

  typeName(): string {
    if (this.tokenType_) return RawAst.Token.typeNames[this.tokenType_]!
    else return 'Raw'
  }
}
// We haven't had much need to distinguish token types, but it's useful to know that an identifier token's code is a
// valid string for an identifier.
export interface IdentifierOrOperatorIdentifierToken extends Token {
  code(): IdentifierOrOperatorIdentifier
}
export interface IdentifierToken extends Token {
  code(): Identifier
}

declare const qualifiedNameBrand: unique symbol
declare const identifierBrand: unique symbol
declare const operatorBrand: unique symbol

/** A string representing a valid qualified name of our language.
 *
 * In our language, the segments are separated by `.`. All the segments except the last must be lexical identifiers. The
 * last may be an identifier or a lexical operator. A single identifier is also a valid qualified name.
 */
export type QualifiedName = string & { [qualifiedNameBrand]: never }

/** A string representing a lexical identifier. */
export type Identifier = string & { [identifierBrand]: never; [qualifiedNameBrand]: never }

/** A string representing a lexical operator. */
export type Operator = string & { [operatorBrand]: never; [qualifiedNameBrand]: never }

/** A string that can be parsed as an identifier in some contexts.
 *
 *  If it is lexically an identifier (see `StrictIdentifier`), it can be used as identifier anywhere.
 *
 *  If it is lexically an operator (see `Operator`), it takes the syntactic role of an identifier if it is the RHS of
 *  a `PropertyAccess`, or it is the name of a `Function` being defined within a type. In all other cases, it is not
 *  valid to use a lexical operator as an identifier (rather, it will usually parse as an `OprApp` or `UnaryOprApp`).
 */
export type IdentifierOrOperatorIdentifier = Identifier | Operator

/** Returns true if `code` can be used as an identifier in some contexts.
 *
 *  If it is lexically an identifier (see `isIdentifier`), it can be used as identifier anywhere.
 *
 *  If it is lexically an operator (see `isOperator`), it takes the syntactic role of an identifier if it is the RHS of
 *  a `PropertyAccess`, or it is the name of a `Function` being defined within a type. In all other cases, it is not
 *  valid to use a lexical operator as an identifier (rather, it will usually parse as an `OprApp` or `UnaryOprApp`).
 */
export function isIdentifierOrOperatorIdentifier(
  code: string,
): code is IdentifierOrOperatorIdentifier {
  return is_ident_or_operator(code) !== 0
}

/** Returns true if `code` is lexically an identifier. */
export function isIdentifier(code: string): code is Identifier {
  return is_ident_or_operator(code) === 1
}

/** Returns true if `code` is lexically an operator. */
export function isOperator(code: string): code is Operator {
  return is_ident_or_operator(code) === 2
}

/** @internal */
export function isTokenId(t: SyncTokenId | AstId | Ast | Owned<Ast> | Owned): t is SyncTokenId {
  return typeof t === 'object' && !(t instanceof Ast)
}
