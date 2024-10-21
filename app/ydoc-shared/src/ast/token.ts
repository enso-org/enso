import type { AstId, DeepReadonly, NodeChild, Owned } from '.'
import { Ast, newExternalId } from '.'
import { assert } from '../util/assert'
import type { ExternalId } from '../yjsModel'
import { isUuid } from '../yjsModel'
import { is_ident_or_operator } from './ffi'
import * as RawAst from './generated/ast'

/** Whether the given value is a {@link Token}. */
export function isToken(maybeToken: unknown): maybeToken is Token {
  return maybeToken instanceof Token
}

/** Whether the given {@link NodeChild} is a {@link NodeChild}<{@link Token}>. */
export function isTokenChild(child: NodeChild<unknown>): child is NodeChild<Token> {
  return isToken(child.node)
}

declare const brandTokenId: unique symbol
export type TokenId = ExternalId & { [brandTokenId]: never }

function newTokenId(): TokenId {
  return newExternalId() as TokenId
}

/** @internal */
export interface SyncTokenId {
  readonly id: TokenId
  readonly code_: string
  readonly tokenType_: RawAst.Token.Type | undefined
}

/** A structure representing a lexical source code unit in the AST. */
export class Token implements SyncTokenId {
  readonly id: TokenId
  code_: string
  tokenType_: RawAst.Token.Type | undefined

  private constructor(code: string, type: RawAst.Token.Type | undefined, id: TokenId) {
    this.id = id
    this.code_ = code
    this.tokenType_ = type
  }

  /** The id of this token. */
  get externalId(): TokenId {
    return this.id
  }

  /** Construct a {@link Token} without a {@link TokenId}. */
  static new(code: string, type?: RawAst.Token.Type) {
    return new this(code, type, newTokenId())
  }

  /** Construct a {@link Token} with a {@link TokenId}. */
  static withId(code: string, type: RawAst.Token.Type | undefined, id: TokenId) {
    assert(isUuid(id))
    return new this(code, type, id)
  }

  /** Whether one {@link SyncTokenId} is equal to another. */
  static equal(a: SyncTokenId, b: SyncTokenId): boolean {
    return a.tokenType_ === b.tokenType_ && a.code_ === b.code_
  }

  /** The code represented by this token. */
  code(): string {
    return this.code_
  }

  /** The name of the token type of this token. */
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
declare const typeOrConsIdentifierBrand: unique symbol
declare const operatorBrand: unique symbol

/**
 * A string representing a valid qualified name of our language.
 *
 * In our language, the segments are separated by `.`. All the segments except the last must be lexical identifiers. The
 * last may be an identifier or a lexical operator. A single identifier is also a valid qualified name.
 */
export type QualifiedName = string & { [qualifiedNameBrand]: never }

/** A string representing a lexical identifier. */
export type Identifier = string & { [identifierBrand]: never; [qualifiedNameBrand]: never }

/** A specific subtype of capitalized identifier, used for type and constructor names. */
export type TypeOrConstructorIdentifier = Identifier & { [typeOrConsIdentifierBrand]: never }

/** A string representing a lexical operator. */
export type Operator = string & { [operatorBrand]: never; [qualifiedNameBrand]: never }

/**
 * A string that can be parsed as an identifier in some contexts.
 *
 *  If it is lexically an identifier (see `StrictIdentifier`), it can be used as identifier anywhere.
 *
 *  If it is lexically an operator (see `Operator`), it takes the syntactic role of an identifier if it is the RHS of
 *  a `PropertyAccess`, or it is the name of a `Function` being defined within a type. In all other cases, it is not
 *  valid to use a lexical operator as an identifier (rather, it will usually parse as an `OprApp` or `UnaryOprApp`).
 */
export type IdentifierOrOperatorIdentifier = Identifier | Operator

/**
 * Returns true if `code` can be used as an identifier in some contexts.
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

/** Whether the given code is lexically an identifier. */
export function isIdentifier(code: string): code is Identifier {
  return is_ident_or_operator(code) === 1
}

/**
 * Whether the given code is a type or constructor identifier.
 * This is true if the code is an identifier beginning with an uppercase letter.
 */
export function isTypeOrConsIdentifier(code: string): code is TypeOrConstructorIdentifier {
  const isUppercase = (s: string) => s.toUpperCase() === s && s.toLowerCase() !== s
  return isIdentifier(code) && code.length > 0 && isUppercase(code[0]!)
}

/** The code as an {@link Identifier} if it is an {@link Identifier}, else `undefined`. */
export function identifier(code: string): Identifier | undefined {
  if (isIdentifier(code)) return code
}

/** Whether the given code is lexically an operator. */
export function isOperator(code: string): code is Operator {
  return is_ident_or_operator(code) === 2
}

/** @internal */
export function isTokenId(
  t: DeepReadonly<SyncTokenId | AstId | Ast | Owned<Ast> | Owned>,
): t is DeepReadonly<SyncTokenId> {
  return typeof t === 'object' && !(t instanceof Ast)
}
