import {
  type MethodCompletionInfo,
  useCompletionData,
} from '@/util/codemirror/language/tableExpression/completionData'
import type {
  CompletionContext,
  CompletionResult,
  CompletionSource,
} from '@codemirror/autocomplete'
import { completionTypeAt } from 'lezer-enso-table-expr'
export type { MethodCompletionInfo }

/** Creates a {@link CompletionSource} from the provided data. */
export function completionSource(
  methods: (() => MethodCompletionInfo[]) | undefined,
  columns: (() => string[]) | undefined,
): CompletionSource {
  const {
    valueOptions,
    valueOptionsStartingWithIdentifier,
    methodOptions,
    columnOptions,
    columnsWithBracket,
  } = useCompletionData(methods, columns)
  return (context: CompletionContext): CompletionResult | null => {
    const completion = completionTypeAt(context.pos, context.state)
    if (!completion) return null
    if (completion.auto === false && !context.explicit) return null
    const options =
      completion.type === 'value' ? valueOptions.value
      : completion.type === 'functionName' ?
        completion.insertDelim ?
          valueOptionsStartingWithIdentifier.value
        : methodOptions.value.methods
      : completion.type === 'columnName' ?
        completion.insertDelim ?
          columnsWithBracket.value
        : columnOptions.value
      : completion.type === 'binop' ?
        [...methodOptions.value.binaryOperators, ...methodOptions.value.postfixOperators]
      : null
    if (options == null) return null
    return { from: completion.pos ?? context.pos, options }
  }
}
