import { LanguageSupport, LRLanguage, type Language } from '@codemirror/language'
import { parser } from './generated/parser'
import { highlight } from './highlight'
export { completionTypeAt } from './autocomplete'

export const tableExpressionLang: Language = LRLanguage.define({
  name: 'table-expression',
  parser: parser.configure({ props: [highlight] }),
})
const langSupport = new LanguageSupport(tableExpressionLang, [])

/** @returns A CodeMirror extension supporting the Enso Table Expression DSL. */
export function tableExpression(): LanguageSupport {
  return langSupport
}
