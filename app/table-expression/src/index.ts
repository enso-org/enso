import { LanguageSupport, LRLanguage } from '@codemirror/language'
import { useCompletions, type MethodCompletionInfo } from './autocomplete'
import { parser } from './generated/parser'
import { highlight } from './highlight'

export type { MethodCompletionInfo }
export interface TableExpressionOptions {
  methods?: () => MethodCompletionInfo[]
  columns?: () => string[]
}

/** @returns A CodeMirror extension supporting the Enso Table Expression DSL. */
export function tableExpression({ methods, columns }: TableExpressionOptions): LanguageSupport {
  const lang = LRLanguage.define({
    name: 'table-expression',
    parser: parser.configure({ props: [highlight] }),
    languageData: {
      autocomplete: useCompletions(methods, columns),
    },
  })
  return new LanguageSupport(lang, [])
}
