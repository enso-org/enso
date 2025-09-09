import type { Completion, CompletionContext, CompletionResult } from '@codemirror/autocomplete'
import { computed } from 'vue'
import { completionTypeAt, type NameCompletion } from './completionType'

/** Completion information for a method. */
export interface MethodCompletionInfo {
  name: string
  description?: string | undefined
  args: boolean
}

interface StringCompletion extends Completion {
  apply: string
}

function getMethodOptions(infos: MethodCompletionInfo[]) {
  const methods: StringCompletion[] = []
  const methodsWithParen: StringCompletion[] = []
  const binaryOperators: Completion[] = []
  for (const { name, description, args } of infos) {
    if (/^[a-z]/.test(name)) {
      const baseCompletion = {
        label: name,
        type: 'method',
        ...(description ? { detail: description } : {}),
        apply: name,
      }
      methods.push(baseCompletion)
      methodsWithParen.push({ ...baseCompletion, apply: name + (args ? '(' : '()') })
    } else {
      binaryOperators.push({ label: name, type: 'operator' })
    }
  }
  return {
    methods,
    methodsWithParen,
    binaryOperators,
  }
}

function applyMapper(f: (s: string) => string): (completion: StringCompletion) => StringCompletion {
  return (completion) => ({ ...completion, apply: f(completion.apply) })
}
const closeBracketAfter = applyMapper((s) => `${s}]`)
const encloseBrackets = applyMapper((s) => `[${s}]`)

/** @returns a function that can be used as a completion provider. */
export function useCompletions(
  methods: (() => MethodCompletionInfo[]) | undefined,
  columns: (() => string[]) | undefined,
) {
  const methodOptions = computed(() => getMethodOptions(methods?.() ?? []))
  const columnOptions = computed(() =>
    Array.from(columns?.() ?? [], (column) => ({
      label: column,
      type: 'variable',
      boost: 1,
      apply: escapeColumn(column),
    })),
  )
  const columnsWithBracket = computed(() => columnOptions.value.map(closeBracketAfter))
  const valueOptions = computed(() => [
    ...methodOptions.value.methodsWithParen,
    ...columnOptions.value.map(encloseBrackets),
  ])

  return (context: CompletionContext): CompletionResult | null => {
    const completion = completionTypeAt(context.pos, context.state)
    if (!completion) return null
    return (
      completion.type === 'value' ? { from: context.pos, options: valueOptions.value }
      : completion.type === 'functionName' ?
        nameCompletions(
          completion,
          () =>
            completion.insertDelim ?
              methodOptions.value.methodsWithParen
            : methodOptions.value.methods,
          context,
        )
      : completion.type === 'columnName' ?
        nameCompletions(
          completion,
          () => (completion.insertDelim ? columnsWithBracket.value : columnOptions.value),
          context,
        )
      : null
    )
  }
}

/** @internal */
export function escapeColumn(column: string) {
  return column.replace(/]/g, ']]')
}

function nameCompletions(
  { pos, auto }: NameCompletion,
  options: () => StringCompletion[],
  context: CompletionContext,
) {
  if (!auto && !context.explicit) return null
  return {
    from: pos,
    options: options(),
  }
}
