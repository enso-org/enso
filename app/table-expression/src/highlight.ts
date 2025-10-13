import { styleTags, tags as t } from '@lezer/highlight'

export const highlight = styleTags({
  Number: t.number,
  ArithOp: t.arithmeticOperator,
  Regex: t.quote,
  LiteralKeyword: t.keyword,
  Date: t.string,
  Column: t.variableName,
  String: t.string,
  Atom: t.atom,
  TypeOp: t.typeOperator,
  CompareOp: t.compareOperator,
  LogicOp: t.logicOperator,
  ControlOp: t.controlOperator,
  Punctuation: t.punctuation,
  OpenParen: t.paren,
  CloseParen: t.paren,
  OpenBracket: t.squareBracket,
  CloseBracket: t.squareBracket,
  Quote: t.quote,
})
