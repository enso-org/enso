import { ensoMarkdown } from '@/components/MarkdownEditor/markdown'
import { assert } from '@/util/assert'
import { toggleHeader, toggleQuote } from '@/util/codemirror/index'
import { setVueHost } from '@/util/codemirror/vueHostExt'
import { EditorState } from '@codemirror/state'
import { Decoration, EditorView } from '@codemirror/view'
import { expect, test } from 'vitest'
import { debugTree, markdownParser } from 'ydoc-shared/ast/ensoMarkdown'

function decorations<T>(
  source: string,
  recognize: (from: number, to: number, decoration: Decoration) => T | undefined,
) {
  const view = new EditorView({
    state: EditorState.create({
      doc: source,
      extensions: ensoMarkdown(),
    }),
  })
  const vueHost = {
    register: () => ({
      unregister: () => {},
      update: () => {},
    }),
    teleportations: new Map(),
  }
  view.dispatch({ effects: setVueHost.of(vueHost) })
  const decorationSets = view.state.facet(EditorView.decorations)
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
        title: deco.spec.attributes.title,
      }
    }
  })
}

function images(source: string) {
  return decorations(source, (from, to, deco) => {
    if ('widget' in deco.spec && 'props' in deco.spec.widget && 'src' in deco.spec.widget.props) {
      return {
        from,
        to,
        src: deco.spec.widget.props.src,
        alt: deco.spec.widget.props.alt,
      }
    }
  })
}

test.each([
  {
    markdown: '[Link text](https://www.example.com/index.html)',
    expectedLinks: [
      {
        text: 'Link text',
        href: 'https://www.example.com/index.html',
      },
    ],
  },
  {
    markdown: '[Link text](https://www.example.com/index.html "title text")',
    expectedLinks: [
      {
        text: 'Link text',
        href: 'https://www.example.com/index.html',
        title: '"title text"',
      },
    ],
  },
  {
    markdown: '[Link text](<https://www.example.com/index.html>)',
    expectedLinks: [
      {
        text: 'Link text',
        href: 'https://www.example.com/index.html',
      },
    ],
  },
  {
    markdown: '[Link text](<https://www.example.com/Url with spaces.html>)',
    expectedLinks: [
      {
        text: 'Link text',
        href: 'https://www.example.com/Url with spaces.html',
      },
    ],
  },
  {
    markdown: '[Link text](https://www.example.com/Spaces not allowed without angle brackets.html)',
    expectedLinks: [],
  },
  {
    markdown: '[Unclosed url](https://www.example.com/index.html',
    expectedLinks: [],
  },
  {
    markdown: '[](https://www.example.com/index.html)',
    expectedLinks: [],
  },
  {
    markdown: '[With empty URL]()',
    expectedLinks: [],
  },
  {
    markdown: '[With no URL]',
    expectedLinks: [],
  },
  {
    markdown: '[Unclosed',
    expectedLinks: [],
  },
  {
    markdown: '<https://example.com>',
    expectedLinks: [
      {
        text: 'https://example.com',
        href: 'https://example.com',
      },
    ],
  },
  {
    markdown: '<example.com>',
    expectedLinks: [],
  },
])('Link decoration: $markdown', ({ markdown, expectedLinks }) => {
  expect(links(markdown)).toEqual(expectedLinks)
  expect(images(markdown)).toEqual([])
})

test.each([
  {
    markdown: '![Image](https://www.example.com/image.avif)',
    image: {
      src: 'https://www.example.com/image.avif',
      alt: 'Image',
    },
  },
  {
    markdown: '![](https://www.example.com/image.avif)',
    image: {
      src: 'https://www.example.com/image.avif',
      alt: '',
    },
  },
  {
    markdown: '![](<https://www.example.com/The image.avif>)',
    image: {
      src: 'https://www.example.com/The image.avif',
      alt: '',
    },
  },
  {
    markdown: '![](<https://www.example.com/The image.avif)',
    image: null,
  },
  {
    markdown: '![](https://www.example.com/The image.avif)',
    image: null,
  },
  {
    markdown: '![Image](https://www.example.com/image.avif',
    image: null,
  },
  {
    markdown: '![Image]()',
    image: null,
  },
  {
    markdown: '![Image]',
    image: null,
  },
  {
    markdown: '![Image',
    image: null,
  },
])('Image decoration: $markdown', ({ markdown, image }) => {
  expect(links(markdown)).toEqual([])
  expect(images(markdown)).toEqual(
    image == null ?
      []
    : [
        {
          from: markdown.length,
          to: markdown.length,
          src: image.src,
          alt: image.alt,
        },
      ],
  )
})

const setupEditor = (source: string) => {
  const selectionStart = source.indexOf('|')
  const selectionEnd = source.lastIndexOf('|')
  const selection = { anchor: selectionStart, head: selectionEnd }
  const doc = source.replaceAll('|', '')
  const view = new EditorView({
    state: EditorState.create({
      doc,
      extensions: ensoMarkdown(),
      selection,
    }),
  })
  const vueHost = {
    register: () => ({
      unregister: () => {},
      update: () => {},
    }),
    teleportations: new Map(),
  }
  view.dispatch({ effects: setVueHost.of(vueHost) })
  return view
}

interface HeaderTestCase {
  source: string
  headerLevel: number
  expected: string
}

const headerTestCases: HeaderTestCase[] = [
  {
    source: 'Some| text',
    headerLevel: 1,
    expected: '# Some text',
  },
  {
    source: '|Some| text',
    headerLevel: 1,
    expected: '# Some text',
  },
  {
    source: '|Some| text',
    headerLevel: 2,
    expected: '## Some text',
  },
  {
    source: '## |Some text',
    headerLevel: 1,
    expected: '# Some text',
  },
  {
    source: '### |Some text',
    headerLevel: 1,
    expected: '# Some text',
  },
  {
    source: 'Fir|st line\nSecond| line',
    headerLevel: 1,
    expected: '# First line\n# Second line',
  },
  {
    source: '# Fir|st line\n# Second| line',
    headerLevel: 1,
    expected: 'First line\nSecond line',
  },
  {
    source: '# |Header',
    headerLevel: 1,
    expected: 'Header',
  },
  {
    source: '# |Don’t touch this one\n## Touch this one\nMake this one h|eader',
    headerLevel: 1,
    expected: '# Don’t touch this one\n# Touch this one\n# Make this one header',
  },
  {
    source: '```\nSome code\n# Head|er in code block\nMore code\n```',
    headerLevel: 2,
    expected: '```\nSome code\n## Header in code block\nMore code\n```',
  },
  {
    source: '> This is a quote\nHeader| in quote',
    headerLevel: 1,
    expected: '> This is a quote\n# Header in quote',
  },
  {
    source: '1. This is a list item\n2. This is| a future header',
    headerLevel: 1,
    expected: '1. This is a list item\n# 2. This is a future header',
  },
]

test.each(headerTestCases)('markdown headers $source', ({ source, headerLevel, expected }) => {
  const view = setupEditor(source)
  toggleHeader(view, headerLevel)
  expect(view.state.doc.toString()).toEqual(expected)
})

interface TestCase {
  desc?: string
  source: string
  expected: string
}

const quotesTestCases: TestCase[] = [
  {
    desc: 'Create simple quote',
    source: 'This| is a quote',
    expected: '> This is a quote',
  },
  {
    desc: 'Multiline quote',
    source: 'This |is a quote\nThis is anoth|er quote',
    expected: '> This is a quote\nThis is another quote',
  },
  {
    desc: 'Disable quote',
    source: '> This |is a quote',
    expected: 'This is a quote',
  },
  {
    desc: 'Disable multiline quote',
    source: '> This is| a quote\nThis is |another quote\n\nThis is a new paragraph',
    expected: 'This is a quote\nThis is another quote\n\nThis is a new paragraph',
  },
]

test.each(quotesTestCases)('markdown quotes $desc', ({ source, expected }) => {
  const view = setupEditor(source)
  toggleQuote(view)
  expect(view.state.doc.toString()).toEqual(expected)
})
