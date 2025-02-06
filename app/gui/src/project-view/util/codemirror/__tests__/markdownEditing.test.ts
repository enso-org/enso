import { ensoMarkdown } from '@/components/MarkdownEditor/markdown'
import {
  HeaderLevel,
  toggleHeader,
  toggleList,
  toggleQuote,
} from '@/util/codemirror/markdownEditing'
import { EditorState } from '@codemirror/state'
import { EditorView } from '@codemirror/view'
import { expect, test } from 'vitest'

/**
 * Setup editor with selection ranging from the first occurence of '|' in the `source` string to the last occurence of '|'.
 * If there is a single '|', it points at the cursor position.
 */
const setupEditor = (source: string) => {
  const selectionStart = source.indexOf('|')
  const selectionEnd = source.lastIndexOf('|')
  const selection = {
    anchor: selectionStart,
    head: selectionEnd > 0 ? selectionEnd - 1 : selectionEnd,
  }
  const doc = source.replaceAll('|', '')
  const view = new EditorView({
    state: EditorState.create({
      doc,
      extensions: ensoMarkdown(),
      selection,
    }),
  })
  return view
}

interface TestCase {
  desc?: string
  source: string
  expected: string
}

interface HeaderTestCase extends TestCase {
  headerLevel: HeaderLevel
}

const headerTestCases: HeaderTestCase[] = [
  {
    source: 'Some| text',
    headerLevel: 1,
    expected: '# Some text',
  },
  {
    source: '**Bold| text**',
    headerLevel: 1,
    expected: '# **Bold text**',
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
    source: '# **Bo|ld**',
    headerLevel: 1,
    expected: '**Bold**',
  },
  {
    source: '# |Don’t touch this one\n## Touch this one\nMake this one h|eader',
    headerLevel: 1,
    expected: '# Don’t touch this one\n# Touch this one\n# Make this one header',
  },
  {
    source: '```\nSome code\nHead|er in code block\nMore code\n```',
    headerLevel: 1,
    expected: '```\nSome code\n# Header in code block\nMore code\n```',
  },
  {
    source: 'Some paragraph\n```\nSome code\n# Head|er in code block\nMore code\n```',
    headerLevel: 2,
    expected: 'Some paragraph\n```\nSome code\n## Header in code block\nMore code\n```',
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
  {
    desc: 'Enable quote in code block',
    source: '```\nSome code\nThis i|s a quote\nMore code\n```',
    expected: '```\nSome code\n> This is a quote\nMore code\n```',
  },
  {
    desc: 'Enable multiline quote in code block',
    source: '```\nSome code\nThis i|s a quote\nAlso |a quote\nMore code\n```',
    expected: '```\nSome code\n> This is a quote\nAlso a quote\nMore code\n```',
  },
  {
    desc: 'Disable quote in code block',
    source: '```\nSome code\n> This i|s a quote\nMore code\n```',
    expected: '```\nSome code\nThis is a quote\nMore code\n```',
  },
  {
    desc: 'Disable multiline quote in code block',
    source: '```\nSome code\n> This i|s a quote\nAlso a q|uote\n\nMore code\n```',
    expected: '```\nSome code\nThis is a quote\nAlso a quote\n\nMore code\n```',
  },
]

test.each(quotesTestCases)('markdown quotes $desc', ({ source, expected }) => {
  const view = setupEditor(source)
  toggleQuote(view)
  expect(view.state.doc.toString()).toEqual(expected)
})

const unorderedListTestCases: TestCase[] = [
  {
    desc: 'Create unordered list from empty line',
    source: '|',
    expected: '- ',
  },
  {
    desc: 'Create simple unordered list',
    source: '|List item\nList item\nList |item',
    expected: '- List item\n- List item\n- List item',
  },
  {
    desc: 'Disable unordered list',
    source: '- Li|st item\n- List item\n- Lis|t item',
    expected: 'List item\nList item\nList item',
  },
  {
    desc: 'Change ordered list to unordered list',
    source: '1. List| item\n2. List item\n3. Lis|t item',
    expected: '- List item\n- List item\n- List item',
  },
  {
    desc: 'Disable unordered list in code block',
    source: '```\nSome code\n- Lis|t item\nMore code\n```',
    expected: '```\nSome code\nList item\nMore code\n```',
  },
  {
    desc: 'Create unordered list in code block',
    source: '```\nSome code\nLis|t item\nAnother |list item\n```',
    expected: '```\nSome code\n- List item\n- Another list item\n```',
  },
  {
    desc: 'Change ordered list to unordered list in code block',
    source: '```\nSome code\n1. List| item\n2. List item\n3. Lis|t item\nSome paragraph\n```',
    expected: '```\nSome code\n- List item\n- List item\n- List item\nSome paragraph\n```',
  },
  {
    desc: 'Disable unordered list in code block',
    source: '```\nSome code\n- List| item\n- List item\n- Lis|t item\nSome paragraph\n```',
    expected: '```\nSome code\nList item\nList item\nList item\nSome paragraph\n```',
  },
]

test.each(unorderedListTestCases)('markdown unordered list $desc', ({ source, expected }) => {
  const view = setupEditor(source)
  toggleList(view, 'unordered')
  expect(view.state.doc.toString()).toEqual(expected)
})

const orderedListTestCases: TestCase[] = [
  {
    desc: 'Create unordered list from empty line',
    source: '|',
    expected: '1. ',
  },
  {
    desc: 'Create simple ordered list',
    source: 'Li|st item\nList item\nLis|t item',
    expected: '1. List item\n2. List item\n3. List item',
  },
  {
    desc: 'Disable ordered list',
    source: '1. Li|st item\n2. List item\n3. Lis|t item',
    expected: 'List item\nList item\nList item',
  },
  {
    desc: 'Change unordered list to ordered list',
    source: '- List| item\n- List item\n- Lis|t item',
    expected: '1. List item\n2. List item\n3. List item',
  },
  {
    desc: 'Create ordered list in code block',
    source: '```\nSome code\nLis|t item\nAnother |list item\n```',
    expected: '```\nSome code\n1. List item\n2. Another list item\n```',
  },
  {
    desc: 'Change unordered list to ordered list in code block',
    source: '```\nSome code\n- List| item\n- List item\n- Lis|t item\nSome paragraph\n```',
    expected: '```\nSome code\n1. List item\n2. List item\n3. List item\nSome paragraph\n```',
  },
  {
    desc: 'Disable ordered list in code block',
    source: '```\nSome code\n1. List| item\n2. List item\n3. Lis|t item\nSome paragraph\n```',
    expected: '```\nSome code\nList item\nList item\nList item\nSome paragraph\n```',
  },
]

test.each(orderedListTestCases)('markdown ordered list $desc', ({ source, expected }) => {
  const view = setupEditor(source)
  toggleList(view, 'ordered')
  expect(view.state.doc.toString()).toEqual(expected)
})
