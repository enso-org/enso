import { tableExpressionLang } from 'lezer-enso-table-expr'
import { type MethodCompletionInfo, completionSource } from './completionSource'
import { functionDocs } from './functionDocs'
export type { MethodCompletionInfo }

/** A CodeMirror extension adding autocomplete support for the Enso Table Expression DSL. */
export function tableExpressionAutocomplete(options: {
  methods: (() => MethodCompletionInfo[]) | undefined
  columns: (() => string[]) | undefined
}) {
  const completionProvider = tableExpressionLang.data.of({
    autocomplete: completionSource(options.methods, options.columns),
  })
  const docsPopups = functionDocs(options.methods)
  return [completionProvider, docsPopups]
}
