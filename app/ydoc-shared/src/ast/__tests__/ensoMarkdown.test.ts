import { describe, expect, test } from 'vitest'
import { debugTree, type DebugTree } from '../../util/lezer'
import { ensoMarkdownParser, ensoStandardMarkdownParser } from '../ensoMarkdown'

function checkTree({
  source,
  expected,
  not,
}: {
  source: string
  expected: DebugTree
  not?: DebugTree
}) {
  const result = debugTree(ensoMarkdownParser.parse(source), source)
  if (not) expect(result).not.toEqual(not)
  expect(result).toEqual(expected)
}

// === Prerendered newlines ===

// Test cases for prerendered newlines. In order to support live-preview editing, our working representation differs
// from standard Markdown in its treatment of newlines. These tests cover the modifications to the parser to operate on
// this internal representation.

test.each([
  {
    source: 'Newline\nstarts new paragraph',
    expected: ['Document', ['Paragraph', 'Newline'], ['Paragraph', 'starts new paragraph']],
  },
  {
    source: '*No multiline\nitalic*',
    expected: ['Document', ['Paragraph', '*No multiline'], ['Paragraph', 'italic*']],
  },
  {
    source: '- List\n  Non-list child',
    expected: [
      'Document',
      [
        'BulletList',
        ['ListItem', ['ListMark', '- '], ['Paragraph', 'List'], ['Paragraph', 'Non-list child']],
      ],
    ],
  },
  {
    source: '- List\n  - Sublist\n  Non-list child',
    expected: [
      'Document',
      [
        'BulletList',
        [
          'ListItem',
          ['ListMark', '- '],
          ['Paragraph', 'List'],
          ['BulletList', ['ListItem', ['ListMark', '- '], ['Paragraph', 'Sublist']]],
          ['Paragraph', 'Non-list child'],
        ],
      ],
    ],
  },
])('Syntax nonstandardism: Prerendered newlines: $source', checkTree)

// === Whitespace handling ===

// To support live-preview editing, we treat "syntactic" spaces as part of the delimiters so that they can be hidden or
// shown together.

test.each([
  {
    source: '# Header',
    expected: ['Document', ['ATXHeading1', ['HeaderMark', '# ']]],
    not: ['Document', ['ATXHeading1', ['HeaderMark', '#']]],
  },
  {
    source: '## Header',
    expected: ['Document', ['ATXHeading2', ['HeaderMark', '## ']]],
    not: ['Document', ['ATXHeading2', ['HeaderMark', '##']]],
  },
  {
    source: '### Header',
    expected: ['Document', ['ATXHeading3', ['HeaderMark', '### ']]],
    not: ['Document', ['ATXHeading3', ['HeaderMark', '###']]],
  },
  {
    source: '> Quoted',
    expected: ['Document', ['Blockquote', ['QuoteMark', '> '], ['Paragraph', 'Quoted']]],
    not: ['Document', ['Blockquote', ['QuoteMark', '>'], ['Paragraph', 'Quoted']]],
  },
  {
    source: '> Quoted\n> multiline',
    expected: [
      'Document',
      [
        'Blockquote',
        ['QuoteMark', '> '],
        ['Paragraph', 'Quoted'],
        ['QuoteMark', '> '],
        ['Paragraph', 'multiline'],
      ],
    ],
  },
  {
    source: '- Bullet',
    expected: [
      'Document',
      ['BulletList', ['ListItem', ['ListMark', '- '], ['Paragraph', 'Bullet']]],
    ],
    not: ['Document', ['BulletList', ['ListItem', ['ListMark', '-'], ['Paragraph', 'Bullet']]]],
  },
  {
    source: '1. Numbered',
    expected: [
      'Document',
      ['OrderedList', ['ListItem', ['ListMark', '1. '], ['Paragraph', 'Numbered']]],
    ],
    not: ['Document', ['OrderedList', ['ListItem', ['ListMark', '1.'], ['Paragraph', 'Numbered']]]],
  },
  {
    source: '# *Formatted header*',
    expected: [
      'Document',
      [
        'ATXHeading1',
        ['HeaderMark', '# '],
        ['Emphasis', ['EmphasisMark', '*'], ['EmphasisMark', '*']],
      ],
    ],
  },
])('Syntax extension: Delimiter tokens include syntactic spaces: $source', checkTree)

// === "Incomplete" syntax special cases ===

// These cases cover modifications to the parser that improve the editing experience by improving the parsing of
// "incomplete" syntax cases. Some constructs that may occur while editing a document are interpreted by the standard
// parser as a different syntax than the syntax that will be recognized when the edit is completed. In a
// syntax-highlighting editor, or especially in a live-preview editor, these intermediate states can be distracting.
// These tests cover customizations to the parser to either not recognize these cases as a secondary syntax, or to
// recognize the case as the same syntax that will likely be present when the edit is completed.

test.each([
  {
    description: 'Empty bullet not parsed as setext heading',
    source: '-\nEmpty bullet, not a setext heading',
    expected: [
      'Document',
      ['BulletList', ['ListItem', ['ListMark', '-'], ['Paragraph', '']]],
      ['Paragraph', 'Empty bullet, not a setext heading'],
    ],
    not: [
      /* TODO */
    ],
  },
  {
    description: 'Empty bold not parsed as HorizontalRule',
    source: '****',
    // TODO
    expected: ['Document', ['HorizontalRule', '****']],
  },
  {
    description: 'Empty bold+italic not parsed as HorizontalRule',
    source: '******',
    // TODO
    expected: ['Document', ['HorizontalRule', '******']],
  },
  {
    description: 'Empty strikethrough not parsed as FencedCode',
    source: '~~~~',
    // TODO
    expected: ['Document', ['FencedCode', ['CodeMark', '~~~~']]],
  },
])('Syntax extensions: Special cases: $description', checkTree)

// === Generic parser improvements ===

// These cases cover improvements to the parser that are not specific to our use case; they may be considered for
// upstream PR, although AST changes are generally compatibility-breaking.

test.each([
  {
    description: 'LinkMarks distinguished from URL content',
    source: '[Link text](<https://url>)',
    expected: [
      'Document',
      [
        'Paragraph',
        [
          'Link',
          ['LinkMark', '['],
          ['LinkMark', ']'],
          ['LinkMark', '('],
          ['LinkMark', '<'],
          ['URL', 'https://url'],
          ['LinkMark', '>'],
          ['LinkMark', ')'],
        ],
      ],
    ],
    not: [
      'Document',
      [
        'Paragraph',
        [
          'Link',
          ['LinkMark', '['],
          ['LinkMark', ']'],
          ['LinkMark', '('],
          ['URL', '<https://url>'],
          ['LinkMark', ')'],
        ],
      ],
    ],
  },
  {
    description: 'LinkMarks distinguished from URL content in image',
    source: '![Image](<https://url>)',
    expected: [
      'Document',
      [
        'Paragraph',
        [
          'Image',
          ['LinkMark', '!['],
          ['LinkMark', ']'],
          ['LinkMark', '('],
          ['LinkMark', '<'],
          ['URL', 'https://url'],
          ['LinkMark', '>'],
          ['LinkMark', ')'],
        ],
      ],
    ],
    not: [
      'Document',
      [
        'Paragraph',
        [
          'Image',
          ['LinkMark', '!['],
          ['LinkMark', ']'],
          ['LinkMark', '('],
          ['URL', '<https://url>'],
          ['LinkMark', ')'],
        ],
      ],
    ],
  },
])('Standards-compatible AST refinements: $description', checkTree)

// === Standard extensions ===

test.each([
  {
    extension: 'Tables',
    source: '| foo | bar |\n| --- | --- |\n| baz | bim |',
    expected: [
      'Document',
      [
        'Table',
        [
          'TableHeader',
          ['TableDelimiter', '|'],
          ['TableCell', 'foo'],
          ['TableDelimiter', '|'],
          ['TableCell', 'bar'],
          ['TableDelimiter', '|'],
        ],
        ['TableDelimiter', '| --- | --- |'],
        [
          'TableRow',
          ['TableDelimiter', '|'],
          ['TableCell', 'baz'],
          ['TableDelimiter', '|'],
          ['TableCell', 'bim'],
          ['TableDelimiter', '|'],
        ],
      ],
    ],
    not: [
      'Document',
      ['Paragraph', '| foo | bar |'],
      ['Paragraph', '| --- | --- |'],
      ['Paragraph', '| baz | bim |'],
    ],
  },
])('Markdown extensions: $extension', checkTree)

// === Standard syntax cases ===

// These cases are not affected by our parser customizations (except for inclusion of extensions, like Strikethrough).
// They are included here mainly as a reference for lezer-markdown's syntax trees.

test.each([
  {
    source: '**',
    expected: ['Document', ['Paragraph', '**']],
  },
  {
    source: 'Not emphasis without content: ****',
    expected: ['Document', ['Paragraph', 'Not emphasis without content: ****']],
  },
  {
    source: 'Not emphasis without content: ******',
    expected: ['Document', ['Paragraph', 'Not emphasis without content: ******']],
  },
  {
    source: 'Empty strikethrough: ~~~~',
    expected: [
      'Document',
      ['Paragraph', ['Strikethrough', ['StrikethroughMark', '~~'], ['StrikethroughMark', '~~']]],
    ],
  },
  {
    source: '~~Strikethrough with space before close ~~',
    expected: [
      'Document',
      ['Paragraph', ['Strikethrough', ['StrikethroughMark', '~~'], ['StrikethroughMark', '~~']]],
    ],
  },
  {
    source: '*~~Formatting with space before close ~~*',
    expected: [
      'Document',
      [
        'Paragraph',
        [
          'Emphasis',
          ['EmphasisMark', '*'],
          ['Strikethrough', ['StrikethroughMark', '~~'], ['StrikethroughMark', '~~']],
          ['EmphasisMark', '*'],
        ],
      ],
    ],
  },
  {
    source: '~~Strikethrough with ~~nested~~ strikethrough~~',
    expected: [
      'Document',
      [
        'Paragraph',
        [
          'Strikethrough',
          ['StrikethroughMark', '~~'],
          ['Strikethrough', ['StrikethroughMark', '~~'], ['StrikethroughMark', '~~']],
          ['StrikethroughMark', '~~'],
        ],
      ],
    ],
  },
  {
    source: '~~Strikethrough with ~~*nested*~~ formatting~~',
    expected: [
      'Document',
      [
        'Paragraph',
        [
          'Strikethrough',
          ['StrikethroughMark', '~~'],
          [
            'Strikethrough',
            ['StrikethroughMark', '~~'],
            ['Emphasis', ['EmphasisMark', '*'], ['EmphasisMark', '*']],
            ['StrikethroughMark', '~~'],
          ],
          ['StrikethroughMark', '~~'],
        ],
      ],
    ],
  },
  {
    source: 'Not strikethrough: ~~~',
    expected: ['Document', ['Paragraph', 'Not strikethrough: ~~~']],
  },
  {
    source: '*Italic***bold**normal',
    expected: [
      'Document',
      [
        'Paragraph',
        ['Emphasis', ['EmphasisMark', '*'], ['EmphasisMark', '*']],
        ['StrongEmphasis', ['EmphasisMark', '**'], ['EmphasisMark', '**']],
      ],
    ],
  },
  {
    source: 'No* italic *here, just asterisks',
    expected: ['Document', ['Paragraph', 'No* italic *here, just asterisks']],
  },
  {
    source: 'Plain text ** with asterisks in the middle',
    expected: ['Document', ['Paragraph', 'Plain text ** with asterisks in the middle']],
  },
  {
    source: 'Plain text** with asterisks in the middle',
    expected: ['Document', ['Paragraph', 'Plain text** with asterisks in the middle']],
  },
  {
    source: 'Plain text **with asterisks in the middle',
    expected: ['Document', ['Paragraph', 'Plain text **with asterisks in the middle']],
  },
  {
    source: 'Plain text **** with asterisks in the middle',
    expected: ['Document', ['Paragraph', 'Plain text **** with asterisks in the middle']],
  },
  {
    source: 'Plain text**** with asterisks in the middle',
    expected: ['Document', ['Paragraph', 'Plain text**** with asterisks in the middle']],
  },
  {
    source: 'Plain text ****with asterisks in the middle',
    expected: ['Document', ['Paragraph', 'Plain text ****with asterisks in the middle']],
  },
  {
    source: 'W*eird emphasi*s',
    expected: [
      'Document',
      ['Paragraph', ['Emphasis', ['EmphasisMark', '*'], ['EmphasisMark', '*']]],
    ],
  },
  {
    source: '*Actually *nested* italic*',
    expected: [
      'Document',
      [
        'Paragraph',
        [
          'Emphasis',
          ['EmphasisMark', '*'],
          ['Emphasis', ['EmphasisMark', '*'], ['EmphasisMark', '*']],
          ['EmphasisMark', '*'],
        ],
      ],
    ],
  },
  {
    source: '*Nested *italic** with **plain* inside*',
    expected: [
      'Document',
      [
        'Paragraph',
        [
          'Emphasis',
          ['EmphasisMark', '*'],
          ['Emphasis', ['EmphasisMark', '*'], ['EmphasisMark', '*']],
          ['EmphasisMark', '*'],
        ],
        [
          'Emphasis',
          ['EmphasisMark', '*'],
          ['Emphasis', ['EmphasisMark', '*'], ['EmphasisMark', '*']],
          ['EmphasisMark', '*'],
        ],
      ],
    ],
  },
  {
    source: '*Double *italic **and single**** **bold** ****with non** italic* inside*',
    expected: [
      'Document',
      [
        'Paragraph',
        [
          'Emphasis',
          ['EmphasisMark', '*'],
          [
            'Emphasis',
            ['EmphasisMark', '*'],
            ['StrongEmphasis', ['EmphasisMark', '**'], ['EmphasisMark', '**']],
            ['EmphasisMark', '*'],
          ],
          ['EmphasisMark', '*'],
        ],
        ['StrongEmphasis', ['EmphasisMark', '**'], ['EmphasisMark', '**']],
        [
          'Emphasis',
          ['EmphasisMark', '*'],
          [
            'Emphasis',
            ['EmphasisMark', '*'],
            ['StrongEmphasis', ['EmphasisMark', '**'], ['EmphasisMark', '**']],
            ['EmphasisMark', '*'],
          ],
          ['EmphasisMark', '*'],
        ],
      ],
    ],
  },
  {
    source: '*Italic **and*** **bold** ***with non** italic inside*',
    expected: [
      'Document',
      [
        'Paragraph',
        [
          'Emphasis',
          ['EmphasisMark', '*'],
          ['StrongEmphasis', ['EmphasisMark', '**'], ['EmphasisMark', '**']],
          ['EmphasisMark', '*'],
        ],
        ['StrongEmphasis', ['EmphasisMark', '**'], ['EmphasisMark', '**']],
        [
          'Emphasis',
          ['EmphasisMark', '*'],
          ['StrongEmphasis', ['EmphasisMark', '**'], ['EmphasisMark', '**']],
          ['EmphasisMark', '*'],
        ],
      ],
    ],
  },
  {
    source: '***Very emphasized***',
    expected: [
      'Document',
      [
        'Paragraph',
        [
          'Emphasis',
          ['EmphasisMark', '*'],
          ['StrongEmphasis', ['EmphasisMark', '**'], ['EmphasisMark', '**']],
          ['EmphasisMark', '*'],
        ],
      ],
    ],
  },
  {
    source: '***Partly** very emphasized*',
    expected: [
      'Document',
      [
        'Paragraph',
        [
          'Emphasis',
          ['EmphasisMark', '*'],
          ['StrongEmphasis', ['EmphasisMark', '**'], ['EmphasisMark', '**']],
          ['EmphasisMark', '*'],
        ],
      ],
    ],
  },
  {
    source: '***Partly* very emphasized**',
    expected: [
      'Document',
      [
        'Paragraph',
        [
          'StrongEmphasis',
          ['EmphasisMark', '**'],
          ['Emphasis', ['EmphasisMark', '*'], ['EmphasisMark', '*']],
          ['EmphasisMark', '**'],
        ],
      ],
    ],
  },
  {
    source: '****Double bold??****',
    expected: [
      'Document',
      [
        'Paragraph',
        [
          'StrongEmphasis',
          ['EmphasisMark', '**'],
          ['StrongEmphasis', ['EmphasisMark', '**'], ['EmphasisMark', '**']],
          ['EmphasisMark', '**'],
        ],
      ],
    ],
  },
  {
    source: '*No multiline\n\nitalic*',
    expected: ['Document', ['Paragraph', '*No multiline'], ['Paragraph', 'italic*']],
  },
  {
    source: '[*Italic link*](https://example.com)',
    expected: [
      'Document',
      [
        'Paragraph',
        [
          'Link',
          ['LinkMark', '['],
          ['Emphasis', ['EmphasisMark', '*'], ['EmphasisMark', '*']],
          ['LinkMark', ']'],
          ['LinkMark', '('],
          ['URL', 'https://example.com'],
          ['LinkMark', ')'],
        ],
      ],
    ],
  },
  {
    source: '~Not strikethrough~',
    expected: ['Document', ['Paragraph', '~Not strikethrough~']],
  },
  {
    source: '~~Strikethrough~~',
    expected: [
      'Document',
      ['Paragraph', ['Strikethrough', ['StrikethroughMark', '~~'], ['StrikethroughMark', '~~']]],
    ],
  },
  {
    source: '`Inline code`',
    expected: ['Document', ['Paragraph', ['InlineCode', ['CodeMark', '`'], ['CodeMark', '`']]]],
  },
  {
    source: '``Inline code``',
    expected: ['Document', ['Paragraph', ['InlineCode', ['CodeMark', '``'], ['CodeMark', '``']]]],
  },
  {
    // TODO: Per the CommonMark spec, the asterisks here don't form an emphasis node because the one on the right isn't
    //  right-flanking. However, this is a normal intermediate state to reach while editing. We should relax the
    //  end-delimiter requirement: Any *non-left-flanking* delimiter run should be allowed to close a format node.
    source: '*Not parsed as italics because of space *',
    expected: ['Document', ['Paragraph', '*Not parsed as italics because of space *']],
  },
])('Inline formatting: $source', checkTree)

test.each([
  {
    source: 'some text',
    expected: ['Document', ['Paragraph', 'some text']],
  },
  {
    source: '[Link without URL]',
    expected: ['Document', ['Paragraph', ['Link', ['LinkMark', '['], ['LinkMark', ']']]]],
  },
  {
    source: '[Link text](https://url)',
    expected: [
      'Document',
      [
        'Paragraph',
        [
          'Link',
          ['LinkMark', '['],
          ['LinkMark', ']'],
          ['LinkMark', '('],
          ['URL', 'https://url'],
          ['LinkMark', ')'],
        ],
      ],
    ],
  },
  {
    source: '[Link text](https://url*bold here prevents the parens from being a URL*)',
    expected: [
      'Document',
      [
        'Paragraph',
        ['Link', ['LinkMark', '['], ['LinkMark', ']']],
        ['Emphasis', ['EmphasisMark', '*'], ['EmphasisMark', '*']],
      ],
    ],
  },
  {
    source: '[*Italic link text*](https://url)',
    expected: [
      'Document',
      [
        'Paragraph',
        [
          'Link',
          ['LinkMark', '['],
          ['Emphasis', ['EmphasisMark', '*'], ['EmphasisMark', '*']],
          ['LinkMark', ']'],
          ['LinkMark', '('],
          ['URL', 'https://url'],
          ['LinkMark', ')'],
        ],
      ],
    ],
  },
  {
    source: '<https://url>',
    expected: [
      'Document',
      ['Paragraph', ['Autolink', ['LinkMark', '<'], ['URL', 'https://url'], ['LinkMark', '>']]],
    ],
  },
  {
    source: '<https://url/*bold here prevents the brackets from forming a URL*>',
    expected: [
      'Document',
      ['Paragraph', ['Emphasis', ['EmphasisMark', '*'], ['EmphasisMark', '*']]],
    ],
  },
  {
    source: '1. List',
    expected: [
      'Document',
      ['OrderedList', ['ListItem', ['ListMark', '1. '], ['Paragraph', 'List']]],
    ],
  },
  {
    source: '1. List\n   1. Sublist',
    expected: [
      'Document',
      [
        'OrderedList',
        [
          'ListItem',
          ['ListMark', '1. '],
          ['Paragraph', 'List'],
          ['OrderedList', ['ListItem', ['ListMark', '1. '], ['Paragraph', 'Sublist']]],
        ],
      ],
    ],
  },
  {
    source: '- List',
    expected: ['Document', ['BulletList', ['ListItem', ['ListMark', '- '], ['Paragraph', 'List']]]],
  },
  {
    source: '- List\n  - Sublist',
    expected: [
      'Document',
      [
        'BulletList',
        [
          'ListItem',
          ['ListMark', '- '],
          ['Paragraph', 'List'],
          ['BulletList', ['ListItem', ['ListMark', '- '], ['Paragraph', 'Sublist']]],
        ],
      ],
    ],
  },
  {
    source: '```enso\nmain = 42\n```',
    expected: [
      'Document',
      [
        'FencedCode',
        ['CodeMark', '```'],
        ['CodeInfo', 'enso'],
        ['CodeText', 'main = 42'],
        ['CodeMark', '```'],
      ],
    ],
  },
  {
    source: '    main = 42',
    expected: ['Document', ['CodeBlock', ['CodeText', 'main = 42']]],
  },
  {
    source: '',
    expected: ['Document', ''],
  },
  {
    source: 'Text\n```\nCode\n```',
    expected: [
      'Document',
      ['Paragraph', 'Text'],
      ['FencedCode', ['CodeMark', '```'], ['CodeText', 'Code'], ['CodeMark', '```']],
    ],
  },
  {
    source: '```\nCode\n```\n```\nCode\n```',
    expected: [
      'Document',
      ['FencedCode', ['CodeMark', '```'], ['CodeText', 'Code'], ['CodeMark', '```']],
      ['FencedCode', ['CodeMark', '```'], ['CodeText', 'Code'], ['CodeMark', '```']],
    ],
  },
  {
    source: '```\nCode',
    expected: ['Document', ['FencedCode', ['CodeMark', '```'], ['CodeText', 'Code']]],
  },
])('Markdown syntax tree: $source', checkTree)

// === Standard markdown, standard representation ===

// These cases test the behavior of the parser for our (CommonMark-compatible) serialization format.
// Some of them cover behavior that the CommonMark standard leaves implementation-defined, and
// otherwise they serve to document the nodes of the parser's tree.

describe('Soft breaks', () => {
  test.each([
    'Paragraph\ncontinuation',
    '- Bullet list\ncontinuation',
    '1. Numbered list\ncontinuation',
  ])('Soft break continues element: $source', (source) => {
    const blockElements = debugTree(ensoStandardMarkdownParser.parse(source), source).length - 1
    expect(blockElements).toBe(1)
  })

  test.each([
    '# Header\nParagraph',
    '```\nFenced code\n```\nParagraph',
    '    Indented code\nParagraph',
    '    Block quote\nParagraph',
    '- List\n# Header',
  ])('Soft break ends element: $source', (source) => {
    const blockElements = debugTree(ensoStandardMarkdownParser.parse(source), source).length - 1
    expect(blockElements).toBe(2)
  })
})

// === YAML frontmatter ===

describe('YAML frontmatter', () => {
  test.each`
    source                                             | expectedContent
    ${'---\nsomething: false\n---\nParagraph'}         | ${'something: false'}
    ${'   --- \nsomething: false\n\n\n---\nParagraph'} | ${'something: false'}
    ${'---\n---\nParagraph'}                           | ${''}
  `('YAML frontmatter is parsed succesfully: $source', ({ source, expectedContent }) => {
    const tree = debugTree(ensoStandardMarkdownParser.parse(source), source)
    expectYamlFrontmatter(tree, expectedContent)
  })

  test('Unfinished YAML frontmatter', () => {
    const source = '---\nParagraph'
    const tree = debugTree(ensoStandardMarkdownParser.parse(source), source)
    expect(tree.length).toBe(2)
    expect(tree[1]).toBe(source)
  })

  test('YAML frontmatter must be at the beginning of the document', () => {
    const source = 'Paragraph\n---\n---\nParagraph'
    const tree = debugTree(ensoStandardMarkdownParser.parse(source), source)
    expect(tree.length).toBe(5)
    for (const node of tree) {
      expect(node).not.toContain('YAMLFrontMatter')
    }
  })

  test('Only a single YAML frontmatter is parsed', () => {
    const source = '---\nsomething: false\n---\nParagraph\n---\n---\nParagraph'
    const tree = debugTree(ensoStandardMarkdownParser.parse(source), source)
    expect(tree.length).toBe(6)
    expectYamlFrontmatter(tree, 'something: false')
    for (let i = 2; i < tree.length; i++) {
      expect(tree[i]).not.toContain('YAMLFrontMatter')
    }
  })
})

function getStringOrValue(value: unknown): string {
  return typeof value === 'string' ? value.trim() : String(value)
}

function expectYamlFrontmatter(tree: DebugTree, expectedContent: string) {
  expect(tree[1]).toContain('YAMLFrontMatter')
  const yamlFrontmatter = tree[1]!
  expect(yamlFrontmatter.length).toBe(4)
  expect(yamlFrontmatter[0]).toBe('YAMLFrontMatter')
  expect(yamlFrontmatter[1]?.[0]).toBe('YAMLMarker')
  expect(yamlFrontmatter[2]?.[0]).toBe('YAMLContent')
  expect(yamlFrontmatter[3]?.[0]).toBe('YAMLMarker')

  expect(getStringOrValue(yamlFrontmatter[1]?.[1])).toBe('---')
  expect(getStringOrValue(yamlFrontmatter[2]?.[1])).toBe(expectedContent)
  expect(getStringOrValue(yamlFrontmatter[3]?.[1])).toBe('---')
}
