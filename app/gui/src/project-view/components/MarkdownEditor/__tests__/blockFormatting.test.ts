import { printTestInput, setupEditor } from '@/components/MarkdownEditor/__tests__/testInput'
import {
  getBlockType,
  insertCodeBlock,
  removeCodeBlock,
  setBlockType,
} from '@/components/MarkdownEditor/codemirror/formatting/block'
import {
  type DelimitedBlockType,
  type SupportedBlockType,
} from '@/components/MarkdownEditor/markdown/types'
import { EditorView } from '@codemirror/view'
import * as objects from 'enso-common/src/utilities/data/object'
import { expect, test } from 'vitest'

const blockFormatCases = [
  {
    formatted: 'Normal text|',
    type: 'Paragraph',
  },
  {
    formatted: '# Header|',
    type: 'ATXHeading1',
    paragraph: 'Header|',
  },
  {
    formatted: '## Header|',
    type: 'ATXHeading2',
    paragraph: 'Header|',
  },
  {
    formatted: '### Header|',
    type: 'ATXHeading3',
    paragraph: 'Header|',
  },
  {
    formatted: '- Bullet list|',
    type: 'BulletList',
    paragraph: 'Bullet list|',
  },
  {
    formatted: '23. Ordered list|',
    type: 'OrderedList',
    paragraph: 'Ordered list|',
    normalized: '1. Ordered list|',
  },
  {
    formatted: '> Quoted text|',
    type: 'Blockquote',
    paragraph: 'Quoted text|',
  },
  {
    formatted: '# |Header 1\n# Header 1|',
    type: 'ATXHeading1',
    paragraph: '|Header 1\nHeader 1|',
  },
  {
    formatted: '# |Header 1\n## Header 2\n### Header 3|',
    type: undefined,
    paragraph: '|Header 1\nHeader 2\nHeader 3|',
  },
] satisfies {
  paragraph?: string
  type: (SupportedBlockType & DelimitedBlockType) | undefined
  formatted: string
  normalized?: string
}[]
const nonParagraphCases = blockFormatCases.filter(({ type }) => type && type !== 'Paragraph')

test.each(blockFormatCases)('Get block type: $source', ({ formatted, type }) =>
  expect(getBlockType(setupEditor(formatted).state)).toEqual(type),
)
test.each(nonParagraphCases)('Remove block formatting: $formatted', ({ formatted, paragraph }) => {
  const view = setupEditor(formatted)
  view.dispatch(setBlockType(view.state, 'Paragraph'))
  expect(printTestInput(view.state.doc.toString(), view.state.selection.main)).toEqual(paragraph)
  expect(getBlockType(view.state)).toEqual('Paragraph')
})
test.each(nonParagraphCases)(
  'Add block formatting: $formatted',
  ({ formatted, type, paragraph, normalized }) => {
    const view = setupEditor(paragraph!)
    view.dispatch(setBlockType(view.state, type!))
    expect(printTestInput(view.state.doc.toString(), view.state.selection.main)).toEqual(
      normalized ?? formatted,
    )
  },
)

const crossTypeCasesInputs = [
  {
    source: '# Single| line',
    formats: {
      ATXHeading2: '## Single| line',
      ATXHeading3: '### Single| line',
      Blockquote: '> Single| line',
      OrderedList: '1. Single| line',
      BulletList: '- Single| line',
    },
  },
  {
    source: '# Multiple| lines\n# Same| format',
    formats: {
      ATXHeading2: '## Multiple| lines\n## Same| format',
      ATXHeading3: '### Multiple| lines\n### Same| format',
      Blockquote: '> Multiple| lines\n> Same| format',
      OrderedList: '1. Multiple| lines\n1. Same| format',
      BulletList: '- Multiple| lines\n- Same| format',
    },
  },
  {
    source: '# Multiple| lines\n## Different| formats',
    formats: {
      ATXHeading1: '# Multiple| lines\n# Different| formats',
      ATXHeading2: '## Multiple| lines\n## Different| formats',
      ATXHeading3: '### Multiple| lines\n### Different| formats',
      Blockquote: '> Multiple| lines\n> Different| formats',
      OrderedList: '1. Multiple| lines\n1. Different| formats',
      BulletList: '- Multiple| lines\n- Different| formats',
    },
  },
  {
    source: '- Multi-|line\n- Block| input',
    formats: {
      ATXHeading1: '# Multi-|line\n# Block| input',
      ATXHeading2: '## Multi-|line\n## Block| input',
      ATXHeading3: '### Multi-|line\n### Block| input',
      Blockquote: '> Multi-|line\n> Block| input',
      OrderedList: '1. Multi-|line\n1. Block| input',
    },
  },
] satisfies { source: string; formats: Partial<Record<SupportedBlockType, string>> }[]
const crossTypeCases = crossTypeCasesInputs.map(({ source, formats }) =>
  objects.unsafeEntries(formats).map((input) => {
    const [type, formatted] = input!
    return {
      source,
      type: type!,
      formatted: formatted!,
    }
  }),
)
test.each(crossTypeCases)('Format-to-format: $formatted', ({ source, type, formatted }) => {
  const view = setupEditor(source)
  view.dispatch(setBlockType(view.state, type))
  expect(printTestInput(view.state.doc.toString(), view.state.selection.main)).toEqual(formatted)
})

/** Supported header levels. */
type HeaderLevel = 1 | 2 | 3

function toggleHeader(view: EditorView, level: HeaderLevel) {
  const headerType = `ATXHeading${level}` as DelimitedBlockType & SupportedBlockType
  const newType = getBlockType(view.state) === headerType ? 'Paragraph' : headerType
  view.dispatch(setBlockType(view.state, newType))
}

function toggleList(view: EditorView, listType: 'BulletList' | 'OrderedList') {
  const newType = getBlockType(view.state) === listType ? 'Paragraph' : listType
  view.dispatch(setBlockType(view.state, newType))
}

function toggleQuote(view: EditorView) {
  const newType = getBlockType(view.state) === 'Blockquote' ? 'Paragraph' : 'Blockquote'
  view.dispatch(setBlockType(view.state, newType))
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
    source: '|Some text',
    headerLevel: 1,
    expected: '# Some text',
  },
  {
    source: 'Some text|',
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
    source: '> This is a quote\nHeader| in quote',
    headerLevel: 1,
    expected: '> This is a quote\n# Header in quote',
  },
  {
    source: '1. This is a list item\n2. This is| a future header',
    headerLevel: 1,
    expected: '1. This is a list item\n# This is a future header',
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
    expected: '> This is a quote\n> This is another quote',
  },
  {
    desc: 'Disable quote',
    source: '> This |is a quote',
    expected: 'This is a quote',
  },
  {
    desc: 'Disable multiline quote',
    source: '> This is| a quote\n> This is |another quote\n\nThis is a new paragraph',
    expected: 'This is a quote\nThis is another quote\n\nThis is a new paragraph',
  },
]

test.each(quotesTestCases)('markdown quotes $desc', ({ source, expected }) => {
  const view = setupEditor(source)
  toggleQuote(view)
  expect(view.state.doc.toString()).toEqual(expected)
})

const unorderedListTestCases: TestCase[] = [
  {
    desc: 'Create unordered list from empty document',
    source: '|',
    expected: '- ',
  },
  {
    desc: 'Create unordered list from empty line',
    source: 'Some text\n|',
    expected: 'Some text\n- ',
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
]

test.each(unorderedListTestCases)('markdown unordered list $desc', ({ source, expected }) => {
  const view = setupEditor(source)
  toggleList(view, 'BulletList')
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
    expected: '1. List item\n1. List item\n1. List item',
  },
  {
    desc: 'Disable ordered list',
    source: '1. Li|st item\n2. List item\n3. Lis|t item',
    expected: 'List item\nList item\nList item',
  },
  {
    desc: 'Change unordered list to ordered list',
    source: '- List| item\n- List item\n- Lis|t item',
    expected: '1. List item\n1. List item\n1. List item',
  },
]

test.each(orderedListTestCases)('markdown ordered list $desc', ({ source, expected }) => {
  const view = setupEditor(source)
  toggleList(view, 'OrderedList')
  expect(view.state.doc.toString()).toEqual(expected)
})

test.each([
  {
    source: '|',
    expected: '```\n|\n```',
  },
  {
    source: 'Paragraph |before',
    expected: 'Paragraph before\n```\n|\n```',
  },
  {
    source: 'Paragraph before|',
    expected: 'Paragraph before\n```\n|\n```',
  },
  {
    source: '|Paragraph before',
    expected: 'Paragraph before\n```\n|\n```',
  },
  {
    source: 'Paragraph |before\nParagraph after',
    expected: 'Paragraph before\n```\n|\n```\nParagraph after',
  },
  {
    source: '# Heading |before\n# Heading after',
    expected: '# Heading before\n```\n|\n```\n# Heading after',
  },
  {
    source: '- List|\n- Before',
    expected: '- List\n- Before\n```\n|\n```',
  },
  {
    source: '```\nCode before|\n```',
    expected: '```\nCode before\n```\n```\n|\n```',
  },
])('Insert code block: $source', ({ source, expected }) => {
  const view = setupEditor(source)
  view.dispatch(insertCodeBlock(view.state))
  expect(getBlockType(view.state)).toBe('FencedCode')
  expect(printTestInput(view.state.doc.toString(), view.state.selection.main)).toEqual(expected)
})

test.each([
  {
    source: 'Text before\n|Selected text|\nText after',
    expected: 'Text before\n```\n|Selected text|\n```\nText after',
  },
  {
    source: 'Text before\nSelected |text|\nText after',
    expected: 'Text before\n```\nSelected |text|\n```\nText after',
  },
  {
    source: 'Text before\n|Selected| text\nText after',
    expected: 'Text before\n```\n|Selected| text\n```\nText after',
  },
  {
    source: 'Text before\nSelected |text\non multiple| lines\nText after',
    expected: 'Text before\n```\nSelected |text\non multiple| lines\n```\nText after',
  },
])('Convert to code block: $source', ({ source, expected }) => {
  const view = setupEditor(source)
  view.dispatch(insertCodeBlock(view.state))
  expect(getBlockType(view.state)).toBe('FencedCode')
  expect(printTestInput(view.state.doc.toString(), view.state.selection.main)).toEqual(expected)
})

test.each([
  {
    source: 'Text before\n```\nCode| block\n```\nText after',
    expected: 'Text before\nCode| block\nText after',
  },
  {
    source: 'Text before\n```md\nCode| block\n```\nText after',
    expected: 'Text before\nCode| block\nText after',
  },
  {
    source: 'Text before\n```|\n```\nText after',
    expected: 'Text before|\nText after',
  },
  {
    source: 'Text before\n```|\n```\nText after',
    expected: 'Text before|\nText after',
  },
  {
    source: 'Text before\n```\nUnclosed code|',
    expected: 'Text before\nUnclosed code|',
  },
  {
    source: 'Text before\n```\nUnclosed code|\n',
    expected: 'Text before\nUnclosed code|\n',
  },
])('Remove code block: $source', ({ source, expected }) => {
  const view = setupEditor(source)
  expect(getBlockType(view.state)).toBe('FencedCode')
  view.dispatch(removeCodeBlock(view.state))
  expect(printTestInput(view.state.doc.toString(), view.state.selection.main)).toEqual(expected)
})
