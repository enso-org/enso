import { linkifyUrls } from '@/components/PlainTextEditor/linkifyUrls'
import { EditorState } from '@codemirror/state'
import { Decoration, EditorView } from '@codemirror/view'
import { expect, test } from 'vitest'

function decorations<T>(
  source: string,
  recognize: (from: number, to: number, decoration: Decoration) => T | undefined,
) {
  const state = EditorState.create({
    doc: source,
    extensions: [linkifyUrls],
  })
  const view = new EditorView({ state })
  const decorationSets = state.facet(EditorView.decorations)
  const results = []
  for (const decorationSet of decorationSets) {
    const resolvedDecorations =
      decorationSet instanceof Function ? decorationSet(view) : decorationSet
    const cursor = resolvedDecorations.iter()
    while (cursor.value != null) {
      const recognized = recognize(cursor.from, cursor.to, cursor.value)
      if (recognized) results.push(recognized)
      cursor.next()
    }
  }
  return results
}

function links(source: string) {
  return decorations(source, (from, to, deco) => {
    if (deco.spec.tagName === 'a') {
      return {
        text: source.substring(from, to),
        href: deco.spec.attributes.href,
      }
    }
  })
}

// Test that link decorations are created for URLs and emails, with `href` set appropriately. The specific URL and email
// syntaxes recognized are tested separately, in the unit tests for `LINKABLE_URL_REGEX` and `LINKABLE_EMAIL_REGEX`.
test.each([
  {
    text: 'Url: https://www.example.com/index.html',
    expectedLinks: [
      {
        text: 'https://www.example.com/index.html',
        href: 'https://www.example.com/index.html',
      },
    ],
  },
  {
    text: 'Email: user@example.com',
    expectedLinks: [
      {
        text: 'user@example.com',
        href: 'mailto:user@example.com',
      },
    ],
  },
])('Link decoration: $text', ({ text, expectedLinks }) => {
  expect(links(text)).toEqual(expectedLinks)
})
