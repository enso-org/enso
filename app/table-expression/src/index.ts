import { LanguageSupport, LRLanguage } from '@codemirror/language'
import { completions } from './autocomplete'
import { parser } from './generated/parser'
import { highlight } from './highlight'

const lang = LRLanguage.define({
  name: 'table-expression',
  parser: parser.configure({ props: [highlight] }),
  languageData: {
    autocomplete: completions,
  },
})

/** @returns A CodeMirror extension supporting the Enso Table Expression DSL. */
export function tableExpression(): LanguageSupport {
  return new LanguageSupport(lang, [])
}
