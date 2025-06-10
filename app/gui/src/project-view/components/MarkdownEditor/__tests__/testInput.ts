import { ensoMarkdownSyntax } from '@/components/MarkdownEditor/markdown/syntax'
import { assert } from '@/util/assert'
import { EditorState } from '@codemirror/state'
import { EditorView } from '@codemirror/view'

/**
 * Setup editor with selection ranging from the first occurrence of '|' in the `source` string to the last occurrence of
 * '|'. If there is a single '|', it points at the cursor position.
 */
export function setupEditor(source: string) {
  const { doc, selection } = parseTestInput(source)
  return new EditorView({
    state: EditorState.create({
      doc,
      extensions: ensoMarkdownSyntax(),
      selection,
    }),
  })
}

/** Parse a test input with '|' in the source indicating the start and (if non-empty) end of the selection. */
export function parseTestInput(source: string): {
  doc: string
  selection: { anchor: number; head: number }
} {
  const selectionStart = source.indexOf('|')
  assert(selectionStart !== -1)
  const selectionEnd = source.indexOf('|', selectionStart + 1)
  const selection = {
    anchor: selectionStart,
    head: selectionEnd > 0 ? selectionEnd - 1 : selectionStart,
  }
  return { doc: source.replaceAll('|', ''), selection }
}

/**
 * Inverts {@link parseTestInput}. When checking a result against an expected result, doing so in "input" format
 * provides easy-to-read output if the test fails.
 */
export function printTestInput(source: string, selection: { anchor: number; head: number }) {
  if (selection.head !== selection.anchor) {
    const pos = Math.max(selection.anchor, selection.head)
    source = source.slice(0, pos) + '|' + source.slice(pos)
  }
  const pos = Math.min(selection.anchor, selection.head)
  return source.slice(0, pos) + '|' + source.slice(pos)
}
