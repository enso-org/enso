/**
 * @file This module supports maintaining a collection of CodeMirror completions, based on provided
 * method information and the rules of the syntax.
 */
import type { MethodSuggestionEntry } from '$/providers/openedProjects/suggestionDatabase/entry'
import type { Completion } from '@codemirror/autocomplete'
import { record } from 'enso-common/src/utilities/data/object'
import { computed } from 'vue'

/** Completion information for a method. */
export interface MethodCompletionInfo {
  name: string
  description?: string | undefined
  args: boolean
  documentation: MethodSuggestionEntry
}

interface StringCompletion extends Completion {
  apply: string
}

/** @returns Reactive completion options, based on the provided method information. */
export function useCompletionData(
  methods: (() => MethodCompletionInfo[]) | undefined,
  columns: (() => string[]) | undefined,
) {
  const methodOptions = computed(() => getMethodOptions(methods?.() ?? []))
  const columnOptions = computed(() => Array.from(columns?.() ?? [], columnOption))
  const columnsWithBracket = computed(() => columnOptions.value.map(closeBracketAfter))
  const valueOptionsStartingWithIdentifier = computed(() => [
    ...methodOptions.value.methodsWithParen,
    ...methodOptions.value.prefixOperators,
    ...CONSTANTS,
  ])
  const valueOptions = computed(() => [
    ...valueOptionsStartingWithIdentifier.value,
    ...columnOptions.value.map(encloseBrackets),
  ])
  return {
    valueOptions,
    valueOptionsStartingWithIdentifier,
    methodOptions,
    columnOptions,
    columnsWithBracket,
  }
}

/** Prefix operators that don't correspond to any method. */
const BUILTIN_PREFIX_OPS = [
  {
    label: 'if',
    type: 'keyword',
  },
]

/** Method names to treat as operators. */
const OP_METHODS = record<
  string,
  { fixity: 'prefix' | 'infix' | 'postfix'; label: string; type: 'operator' }
>({
  /* eslint-disable camelcase */
  not: { fixity: 'prefix', label: '!', type: 'operator' },
  like: { fixity: 'infix', label: 'like', type: 'operator' },
  between: { fixity: 'infix', label: 'between', type: 'operator' },
  is_in: { fixity: 'infix', label: 'is in', type: 'operator' }, // TODO: insert parens for RHS
  is_nothing: { fixity: 'postfix', label: 'is null', type: 'operator' },
  is_empty: { fixity: 'postfix', label: 'is empty', type: 'operator' },
  /* eslint-enable camelcase */
})

const identity = <T>(x: T) => x

function getMethodOptions(infos: MethodCompletionInfo[]) {
  const methods: StringCompletion[] = []
  const methodsWithParen: StringCompletion[] = []
  const operators: Record<'prefix' | 'postfix' | 'infix', Completion[]> = {
    prefix: [...BUILTIN_PREFIX_OPS],
    postfix: [],
    infix: [],
  }
  for (const { name, description, args } of infos) {
    const withDescription =
      description ?
        <T extends Completion>(completion: T): T => ({ ...completion, detail: description })
      : identity
    if (name in OP_METHODS) {
      const { fixity, label, type } = OP_METHODS[name]!
      operators[fixity].push(withDescription({ label, type }))
    } else if (!/^[a-z]/.test(name)) {
      operators.infix.push(withDescription({ label: name, type: 'operator' }))
    } else {
      const baseCompletion = withDescription({
        label: name,
        type: 'method',
        apply: name,
      })
      methods.push(baseCompletion)
      methodsWithParen.push({ ...baseCompletion, apply: name + (args ? '(' : '()') })
    }
  }
  return {
    methods,
    methodsWithParen,
    binaryOperators: operators.infix,
    prefixOperators: operators.prefix,
    postfixOperators: operators.postfix,
  }
}

function columnOption(column: string) {
  return {
    label: column,
    type: 'variable',
    boost: 1,
    apply: escapeColumn(column),
  }
}

const CONSTANTS = ['true', 'false', 'nothing', 'null'].map((label) => ({
  label,
  type: 'keyword',
}))

function applyMapper(f: (s: string) => string): (completion: StringCompletion) => StringCompletion {
  return (completion) => ({ ...completion, apply: f(completion.apply) })
}
const closeBracketAfter = applyMapper((s) => `${s}]`)
const encloseBrackets = applyMapper((s) => `[${s}]`)

/** @internal */
export function escapeColumn(column: string) {
  return column.replace(/]/g, ']]')
}
