import { ensoMarkdown } from '@/components/MarkdownEditor/markdown'
import { setVueHost } from '@/util/codemirror/vueHostExt'
import { EditorState } from '@codemirror/state'
import { Decoration, EditorView } from '@codemirror/view'
import { expect, test } from 'vitest'

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
