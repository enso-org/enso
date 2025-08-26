import type { Completion, CompletionContext, CompletionResult } from '@codemirror/autocomplete'
import { computed } from 'vue'
import { completionTypeAt, type NameCompletion } from './completionType'

/** Completion information for a method. */
export interface MethodCompletionInfo {
  name: string
  description?: string | undefined
}

interface StringCompletion extends Completion {
  apply: string
}

function getMethodOptions(infos: MethodCompletionInfo[]) {
  const methods: StringCompletion[] = []
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
    binaryOperators,
  }
}

function applyMapper(f: (s: string) => string): (completion: StringCompletion) => StringCompletion {
  return (completion) => ({ ...completion, apply: f(completion.apply) })
}
const openParenAfter = applyMapper((s) => `${s}(`)
const closeBracketAfter = applyMapper((s) => `${s}]`)
const encloseBrackets = applyMapper((s) => `[${s}]`)

/** @returns a function that can be used as a completion provider. */
export function useCompletions(
  methods: (() => MethodCompletionInfo[]) | undefined,
  columns: (() => string[]) | undefined,
) {
  const methodOptions = computed(() => getMethodOptions(methods?.() ?? []))
  const methodsWithParen = computed(() => methodOptions.value.methods.map(openParenAfter))
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
    ...methodsWithParen.value,
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
          () => (completion.insertDelim ? methodsWithParen.value : methodOptions.value.methods),
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
