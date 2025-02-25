import { expect, test } from 'vitest'
import { debugTree, markdownParser } from '../ensoMarkdown'

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
    source: '- List\n  Non-list child',
    expected: [
      'Document',
      ['BulletList', ['ListItem', ['ListMark', '- '], ['Paragraph', 'List\n  Non-list child']]],
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
          [
            'BulletList',
            ['ListItem', ['ListMark', '- '], ['Paragraph', 'Sublist\n  Non-list child']],
          ],
        ],
      ],
    ],
  },
])('Enso Markdown tree structure: $source', ({ source, expected }) => {
  expect(debugTree(markdownParser.parse(source), source)).toEqual(expected)
})
