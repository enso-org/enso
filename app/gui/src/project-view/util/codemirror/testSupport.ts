import { EditorSelection } from '@codemirror/state'
import { type EditorView } from '@codemirror/view'

/** Returns an API for the editor content, used by the integration tests. */
export function testSupport(editorView: EditorView) {
  return {
    textContent: () => editorView.state.doc.toString(),
    textLength: () => editorView.state.doc.length,
    indexOf: (substring: string, position?: number) =>
      editorView.state.doc.toString().indexOf(substring, position),
    placeCursor: (at: number) => {
      editorView.dispatch({ selection: EditorSelection.create([EditorSelection.cursor(at)]) })
    },
    select: (from: number, to: number) => {
      editorView.dispatch({ selection: EditorSelection.create([EditorSelection.range(from, to)]) })
    },
    selectAndReplace: (from: number, to: number, replaceWith: string) => {
      editorView.dispatch({ selection: EditorSelection.create([EditorSelection.range(from, to)]) })
      editorView.dispatch(editorView.state.update(editorView.state.replaceSelection(replaceWith)))
    },
    writeText: (text: string, from: number) => {
      editorView.dispatch({
        changes: [{ from: from, insert: text }],
        selection: { anchor: from + text.length },
      })
    },
  }
}
