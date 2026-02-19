import { singleLineDisplay } from '@/util/codemirror/singleLineDisplay'
import { EditorState } from '@codemirror/state'
import { EditorView } from '@codemirror/view'
import { expect, test } from 'vitest'

function decorations(source: string) {
  const view = new EditorView({
    state: EditorState.create({
      doc: source,
      extensions: singleLineDisplay(),
    }),
  })
  const decorationSets = view.state.facet(EditorView.decorations)
  const results = []
  for (const decorationSet of decorationSets) {
    const resolvedDecorations =
      decorationSet instanceof Function ? decorationSet(view) : decorationSet
    const cursor = resolvedDecorations.iter()
    while (cursor.value != null) {
      results.push({ from: cursor.from, to: cursor.to })
      cursor.next()
    }
  }
  return results
}

test.each([
  { source: '', breaks: [] },
  { source: 'a', breaks: [] },
  { source: 'a\nb', breaks: [{ from: 1, to: 2 }] },
  { source: '\nb', breaks: [{ from: 0, to: 1 }] },
  { source: 'a\n', breaks: [{ from: 1, to: 2 }] },
  {
    source: 'a\nb\n',
    breaks: [
      { from: 1, to: 2 },
      { from: 3, to: 4 },
    ],
  },
  {
    source: 'a\n\n',
    breaks: [
      { from: 1, to: 2 },
      { from: 2, to: 3 },
    ],
  },
])('Line break ranges', ({ source, breaks }) => {
  expect(decorations(source)).toEqual(breaks)
})
