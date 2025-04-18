import { defineLanguageFacet, Language, LanguageSupport } from '@codemirror/language'
import { styleTags, tags as t } from '@lezer/highlight'
import { parser } from './generated/parser'
export { parser }

const data = defineLanguageFacet({})

const highlight = styleTags({
  Number: t.number,
  ArithOp: t.arithmeticOperator,
  Regex: t.quote,
  LiteralKeyword: t.keyword,
  Date: t.string,
  Column: t.string,
  String: t.string,
  Atom: t.atom,
  TypeOp: t.typeOperator,
  CompareOp: t.compareOperator,
  LogicOp: t.logicOperator,
  ControlOp: t.controlOperator,
  Punctuation: t.punctuation,
  Paren: t.paren,
  SquareBracket: t.squareBracket,
  Quote: t.quote,
})

/** @returns A CodeMirror extension supporting the Enso Table Expression DSL. */
export function tableExpression(): LanguageSupport {
  const lang = new Language(data, parser.configure({ props: [highlight] }), [], 'table-expression')
  return new LanguageSupport(lang, [])
}
