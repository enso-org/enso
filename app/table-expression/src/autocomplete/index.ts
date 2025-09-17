import type { CompletionContext, CompletionResult } from '@codemirror/autocomplete'
import { type MethodCompletionInfo, useCompletionData } from './completions'
import { completionTypeAt } from './completionType'
export type { MethodCompletionInfo }

/** @returns a function that can be used as a completion provider. */
export function useCompletions(
  methods: (() => MethodCompletionInfo[]) | undefined,
  columns: (() => string[]) | undefined,
) {
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
