import { expandRangeToIncludeFencedBlocks } from '@/components/MarkdownEditor/markdown/trees'
import { syntaxTree } from '@codemirror/language'
import {
  type EditorSelection,
  type Extension,
  RangeSetBuilder,
  type SelectionRange,
  type Text,
} from '@codemirror/state'
import { Decoration, type DecorationSet, EditorView } from '@codemirror/view'
import type { Tree } from '@lezer/common'
import { Range } from 'ydoc-shared/util/data/range'

/** Extension applying a CSS class to identify the cursor's location in the document, for edit-mode rendering. */
export function cursorDecoratorExt(): Extension {
  return EditorView.decorations.compute(['selection', 'doc'], (state) =>
    cursorDecorations(state.selection, state.doc, syntaxTree(state)),
  )
}

function linesToDecorate(range: Range, doc: Text, tree: Tree): Range {
  const expandedRange = expandRangeToIncludeFencedBlocks(tree, range)
  return Range.unsafeFromBounds(
    doc.lineAt(expandedRange.from).number,
    doc.lineAt(expandedRange.to).number,
  )
}

function selectionRange(selection: SelectionRange): Range {
  return Range.unsafeFromBounds(selection.from, selection.to)
}

function cursorDecorations(selection: EditorSelection, doc: Text, tree: Tree): DecorationSet {
  const builder = new RangeSetBuilder<Decoration>()
  for (const range of selection.ranges) {
    const lineRange = linesToDecorate(selectionRange(range), doc, tree)
    for (let i = lineRange.from; i <= lineRange.to; i++) {
      const line = doc.line(i)
      builder.add(
        line.from,
        line.from,
        Decoration.line({
          class: 'cm-has-cursor',
        }),
      )
    }
  }
  return builder.finish()
}
