import { expect, test } from 'vitest'
import { type DebugTree, debugTree, ensoMarkdownParser } from '../ensoMarkdown'

const miscCases = [
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
  },
  {
    source: '[Link text](<https://url/*not bold*>)',
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
          ['URL', 'https://url/*not bold*'],
          ['LinkMark', '>'],
          ['LinkMark', ')'],
        ],
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
]

function checkTree({ source, expected }: { source: string; expected: DebugTree }) {
  expect(debugTree(ensoMarkdownParser.parse(source), source)).toEqual(expected)
}

test.each(miscCases)('Enso Markdown tree structure: $source', checkTree)

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
    // TODO: We should stop the horizontal rule parser from handling this, because it conflicts with cursor-formatting.
    source: '****',
    expected: ['Document', ['HorizontalRule', '****']],
  },
  {
    // TODO: We should stop the horizontal rule parser from handling this, because it conflicts with cursor-formatting.
    source: '******',
    expected: ['Document', ['HorizontalRule', '******']],
  },
  {
    // TODO: We should stop the code block parser from handling this, because it conflicts with cursor-formatting case.
    source: '~~~~',
    expected: ['Document', ['FencedCode', ['CodeMark', '~~~~']]],
    // expected: ['Document', ['Paragraph', '~~~~']],
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
])('Prerendered newlines: $source', checkTree)
