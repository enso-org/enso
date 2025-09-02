import { syntaxTree } from '@codemirror/language'
import type { EditorState } from '@codemirror/state'
import type { TreeCursor } from '@lezer/common'

export interface NameCompletion {
  type: 'functionName' | 'columnName'
  pos: number
  /**
   * True if the completion dialog shoud be opened automatically; otherwise, the user must manually
   * trigger it.
   */
  auto: boolean
  /**
   * True if a delimiter should be inserted after the completion; false if it is not needed, e.g.
   * when editing a name already followed by a delimiter.
   */
  insertDelim: boolean
}

export interface FunctionInfoCompletion {
  type: 'functionInfo'
  functionName: string
}

export type CompletionType = NameCompletion | FunctionInfoCompletion

const INITIAL_COMPLETION_TYPE: CompletionType = {
  type: 'functionName',
  pos: 0,
  auto: true,
  insertDelim: true,
}

/** Returns information about the completion at the given position. */
export function completionTypeAt(pos: number, state: EditorState): CompletionType | null {
  const doc = state.doc
  if (doc.length === 0) return INITIAL_COMPLETION_TYPE
  const tree = syntaxTree(state)
  const cursor = tree.cursorAt(pos, 1)
  const { child, childOpt, siblingOpt, match } = useParsec(cursor)
  const parseNode = () =>
    match<CompletionType | null>({
      Function: (func) =>
        childOpt('Paren', (paren) =>
          paren ?
            pos <= paren.from ?
              { type: 'functionName', pos: func.from, auto: pos === paren.from, insertDelim: false }
            : { type: 'functionInfo', functionName: doc.sliceString(func.from, paren.from) }
          : { type: 'functionName', pos: func.from, auto: pos === func.to, insertDelim: true },
        ),
      Column: (column) =>
        child('SquareBracket', (open) =>
          siblingOpt('SquareBracket', (close) => ({
            type: 'columnName',
            pos: open.to,
            auto: pos === (close?.from ?? column.to),
            insertDelim: !close,
          })),
        ),
    })
  if (IGNORED_LEAF_NAMES.includes(cursor.name)) cursor.parent()
  const completion = parseNode()
  if (completion) return completion
  if (cursor.moveTo(pos, -1)) {
    const completion = parseNode()
    if (completion) return completion
  }
  return null
}

const IGNORED_LEAF_NAMES = ['Paren', 'Number', 'SquareBracket']

interface Range {
  from: number
  to: number
}

function useParsec(cursor: TreeCursor) {
  function pos(cursor: TreeCursor): Range {
    return { from: cursor.from, to: cursor.to }
  }
  function childOpt<T>(name: string, then: (match: Range | undefined) => T): T {
    return then(cursor.firstChild() && cursor.name === name ? pos(cursor) : undefined)
  }
  function siblingOpt<T>(name: string, then: (match: Range | undefined) => T): T {
    return then(cursor.nextSibling() && cursor.name === name ? pos(cursor) : undefined)
  }
  function child<T>(name: string, then: (match: Range) => T): T | null {
    return childOpt(name, (match) => (match ? then(match) : null))
  }
  function sibling<T>(name: string, then: (match: Range) => T): T | null {
    return siblingOpt(name, (match) => (match ? then(match) : null))
  }
  function match<T>(then: Record<string, (match: Range) => T>): T | null {
    return then[cursor.name]?.(pos(cursor)) ?? null
  }
  return { childOpt, siblingOpt, child, sibling, match }
}
