import { type EditorSelection, type Extension, RangeSetBuilder, type Text } from '@codemirror/state'
import { Decoration, type DecorationSet, EditorView } from '@codemirror/view'

/** Extension applying a CSS class to identify the cursor's location in the document, for edit-mode rendering. */
export function cursorDecoratorExt(): Extension {
  return EditorView.decorations.compute(['selection', 'doc'], (state) =>
    cursorDecorations(state.selection, state.doc),
  )
}

function cursorDecorations(selection: EditorSelection, doc: Text): DecorationSet {
  const builder = new RangeSetBuilder<Decoration>()
  for (const range of selection.ranges) {
    const lineFrom = doc.lineAt(range.from)
    const lineTo = doc.lineAt(range.to)
    for (let i = lineFrom.number; i <= lineTo.number; i++) {
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
