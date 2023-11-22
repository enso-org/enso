import { Token } from '@/generated/ast'
import type { Span, TokenId } from '@/util/ast/abstract'

export abstract class Tok {
  private _code: string
  exprId: TokenId
  readonly _tokenType: Token.Type
  // TODO for #8367: Eliminate this before enabling edit support.
  readonly span: Span
  protected constructor(code: string, id: TokenId, type: Token.Type, span: Span) {
    this._code = code
    this.exprId = id
    this._tokenType = type
    this.span = span
  }

  code(): string {
    return this._code
  }

  typeName(): string {
    return Token.typeNames[this._tokenType]!
  }
}

export function make(code: string, id: TokenId, type: Token.Type, span: Span): Tok {
  switch (type) {
    case Token.Type.Newline:
      return new Newline(code, id, span)
    case Token.Type.OpenSymbol:
      return new OpenSymbol(code, id, span)
    case Token.Type.CloseSymbol:
      return new CloseSymbol(code, id, span)
    case Token.Type.Wildcard:
      return new Wildcard(code, id, span)
    case Token.Type.AutoScope:
      return new AutoScope(code, id, span)
    case Token.Type.Ident:
      return new Ident(code, id, span)
    case Token.Type.Operator:
      return new Operator(code, id, span)
    case Token.Type.Digits:
      return new Digits(code, id, span)
    case Token.Type.NumberBase:
      return new NumberBase(code, id, span)
    case Token.Type.Private:
      return new Private(code, id, span)
    case Token.Type.TextStart:
      return new TextStart(code, id, span)
    case Token.Type.TextSection:
      return new TextSection(code, id, span)
    case Token.Type.TextEnd:
      return new TextEnd(code, id, span)
    case Token.Type.TextEscape:
      return new TextEscape(code, id, span)
    case Token.Type.TextInitialNewline:
      return new TextInitialNewline(code, id, span)
    case Token.Type.TextNewline:
      return new TextNewline(code, id, span)
    case Token.Type.Invalid:
      return new Invalid(code, id, span)
    case Token.Type.BlockStart:
      return new Invalid(code, id, span)
    case Token.Type.BlockEnd:
      return new Invalid(code, id, span)
  }
}

export class Operator extends Tok {
  constructor(code: string, id: TokenId, span: Span) {
    super(code, id, Token.Type.Operator, span)
  }
}
export class Newline extends Tok {
  constructor(code: string, id: TokenId, span: Span) {
    super(code, id, Token.Type.Newline, span)
  }
}
export class OpenSymbol extends Tok {
  constructor(code: string, id: TokenId, span: Span) {
    super(code, id, Token.Type.OpenSymbol, span)
  }
}
export class CloseSymbol extends Tok {
  constructor(code: string, id: TokenId, span: Span) {
    super(code, id, Token.Type.CloseSymbol, span)
  }
}
export class Wildcard extends Tok {
  constructor(code: string, id: TokenId, span: Span) {
    super(code, id, Token.Type.Wildcard, span)
  }
}
export class AutoScope extends Tok {
  constructor(code: string, id: TokenId, span: Span) {
    super(code, id, Token.Type.AutoScope, span)
  }
}
export class Digits extends Tok {
  constructor(code: string, id: TokenId, span: Span) {
    super(code, id, Token.Type.Digits, span)
  }
}
export class NumberBase extends Tok {
  constructor(code: string, id: TokenId, span: Span) {
    super(code, id, Token.Type.NumberBase, span)
  }
}
export class Private extends Tok {
  constructor(code: string, id: TokenId, span: Span) {
    super(code, id, Token.Type.Private, span)
  }
}
export class TextStart extends Tok {
  constructor(code: string, id: TokenId, span: Span) {
    super(code, id, Token.Type.TextStart, span)
  }
}
export class TextEnd extends Tok {
  constructor(code: string, id: TokenId, span: Span) {
    super(code, id, Token.Type.TextEnd, span)
  }
}
export class TextSection extends Tok {
  constructor(code: string, id: TokenId, span: Span) {
    super(code, id, Token.Type.TextSection, span)
  }
}
export class TextEscape extends Tok {
  constructor(code: string, id: TokenId, span: Span) {
    super(code, id, Token.Type.TextEscape, span)
  }
}
export class TextInitialNewline extends Tok {
  constructor(code: string, id: TokenId, span: Span) {
    super(code, id, Token.Type.TextInitialNewline, span)
  }
}
export class TextNewline extends Tok {
  constructor(code: string, id: TokenId, span: Span) {
    super(code, id, Token.Type.TextNewline, span)
  }
}
export class Invalid extends Tok {
  constructor(code: string, id: TokenId, span: Span) {
    super(code, id, Token.Type.Invalid, span)
  }
}
export class Ident extends Tok {
  constructor(code: string, id: TokenId, span: Span) {
    super(code, id, Token.Type.Ident, span)
  }
}

declare const OperatorKey: unique symbol
declare module '@/providers/widgetRegistry' {
  export interface WidgetInputTypes {
    [OperatorKey]: Operator
  }
}
