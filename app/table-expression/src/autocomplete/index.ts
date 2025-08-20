import type { Completion, CompletionContext, CompletionResult } from '@codemirror/autocomplete'
import { computed } from 'vue'
import { completionTypeAt } from './completionType'

/** Completion information for a method. */
export interface MethodCompletionInfo {
  name: string
  description?: string | undefined
}

function getMethodOptions(infos: MethodCompletionInfo[]) {
  const methods: Completion[] = []
  const binaryOperators: Completion[] = []
  for (const { name, description } of infos) {
    if (/^[a-z]/.test(name)) {
      methods.push({
        label: name,
        type: 'method',
        ...(description ? { detail: description } : {}),
        apply: name,
      })
    } else {
      binaryOperators.push({ label: name, type: 'operator' })
    }
  }
  return {
    methods,
    methodsWithParens: methods.map((method) => ({ ...method, apply: `${method.apply}(` })),
    binaryOperators,
  }
}

/** @returns a function that can be used as a completion provider. */
export function useCompletions(methods: (() => MethodCompletionInfo[]) | undefined) {
  const methodOptions = computed(() => getMethodOptions(methods?.() ?? []))

  return (context: CompletionContext): CompletionResult | null => {
    const completion = completionTypeAt(context.pos, context.state)
    if (!completion) return null
    if (completion.type === 'functionName') {
      const { pos, auto, insertDelim } = completion
      if (!auto && !context.explicit) return null
      return {
        from: pos,
        options: insertDelim ? methodOptions.value.methodsWithParens : methodOptions.value.methods,
      }
    }
    return null
  }
}
