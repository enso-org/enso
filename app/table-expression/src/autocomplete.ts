import type { Completion, CompletionContext, CompletionResult } from '@codemirror/autocomplete'
import { syntaxTree } from '@codemirror/language'
import type { EditorState } from '@codemirror/state'
import { computed } from 'vue'

/** Completion information for a method. */
export interface MethodCompletionInfo {
  name: string
  description?: string | undefined
}

function getMethodOptions(infos: MethodCompletionInfo[]) {
  const methods: Completion[] = []
  const binaryOperators: Completion[] = []
  const unaryOperators: Completion[] = []
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
    unaryOperators,
  }
}

/** @returns a function that can be used as a completion provider. */
export function useCompletions(methods: (() => MethodCompletionInfo[]) | undefined) {
  const methodOptions = computed(() => getMethodOptions(methods?.() ?? []))

  return (context: CompletionContext): CompletionResult | null => {
    const completion = completionTypeAt(context.pos, context.state)
    if (!completion) return null
    if (completion.type === 'functionName') {
      const { pos, auto, insertParen } = completion
      if (!auto && !context.explicit) return null
      return {
        from: pos,
        options: insertParen ? methodOptions.value.methodsWithParens : methodOptions.value.methods,
      }
    }
    return null
  }
}

interface FunctionNameCompletion {
  type: 'functionName'
  pos: number
  auto: boolean
  insertParen: boolean
}

interface FunctionInfoCompletion {
  type: 'functionInfo'
  functionName: string
}

type CompletionType = FunctionNameCompletion | FunctionInfoCompletion

const INITIAL_COMPLETION_TYPE: CompletionType = {
  type: 'functionName',
  pos: 0,
  auto: true,
  insertParen: true,
}

/** @internal */
export function completionTypeAt(pos: number, state: EditorState): CompletionType | null {
  if (state.doc.length === 0) return INITIAL_COMPLETION_TYPE
  const tree = syntaxTree(state)

  const cursor = tree.cursorAt(pos, 1)
  if (isIgnoredLeaf(cursor)) cursor.parent()
  if (nameAt(cursor) === 'Function') {
    const functionFrom = cursor.from
    const functionTo = cursor.to
    if (cursor.firstChild()) {
      if (nameAt(cursor) === 'Paren') {
        const parenFrom = cursor.from
        // Cursor in function name, before parens
        return pos <= parenFrom ?
            { type: 'functionName', pos: functionFrom, auto: pos === parenFrom, insertParen: false }
          : { type: 'functionInfo', functionName: state.doc.sliceString(functionFrom, parenFrom) }
      }
      // Fall through to no-parens case. The cursor has been moved, but we don't need it anymore.
    }
    // Cursor in function name, no parens
    return { type: 'functionName', pos: functionFrom, auto: pos === functionTo, insertParen: true }
  }

  // Couldn't find a `Function` with `moveTo` side=1, try side=-1.
  if (cursor.moveTo(pos, -1) && nameAt(cursor) === 'Function') {
    const functionFrom = cursor.from
    // Cursor at end of function name, no parens
    return { type: 'functionName', pos: functionFrom, auto: true, insertParen: true }
  }

  return null
}

/**
 * Access a cursor's `name` field without TS narrowing the type, which breaks because cursors are
 * mutable.
 */
function nameAt({ name }: { name: string }) {
  return name
}

const IGNORED_LEAF_NAMES = ['Paren', 'Number', 'SquareBracket']
function isIgnoredLeaf({ name }: { name: string }) {
  return IGNORED_LEAF_NAMES.includes(name)
}
