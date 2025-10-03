import type { TreeCursor } from '@lezer/common'
import { expect, test } from 'vitest'
import { parser } from '../generated/parser'

/** Represents the structure of a @{link Tree} in a JSON-compatible format. */
type DebugTree = (string | DebugTree)[]

/** @returns A debug representation of the provided {@link Tree} */
function debugTree(tree: { cursor: () => TreeCursor }, doc: string): DebugTree {
  const cursor = tree.cursor()
  let current: (string | DebugTree)[] = []
  const stack: (string | DebugTree)[][] = []
  cursor.iterate(
    (node) => {
      const child: (string | DebugTree)[] = [node.name]
      current.push(child)
      stack.push(current)
      current = child
    },
    (node) => {
      if (current.length === 1) current.push(doc.slice(node.from, node.to))
      current = stack.pop()!
    },
  )
  return current[0]! as DebugTree
}

function numberCase(n: string) {
  return {
    code: n,
    expected: ['Expression', ['Number', n.trim()]],
  }
}

test.each([
  ...['1', '0', '10', '3.14', '0.1', '0.0', '1_000', '1_000.000_1', '1 ', ' 1'].map(numberCase),
  {
    code: '1 + 1',
    expected: ['Expression', ['BinOpApp', ['Number', '1'], ['ArithOp', '+'], ['Number', '1']]],
  },
  {
    code: '1 + 2 + 3',
    expected: [
      'Expression',
      [
        'BinOpApp',
        ['BinOpApp', ['Number', '1'], ['ArithOp', '+'], ['Number', '2']],
        ['ArithOp', '+'],
        ['Number', '3'],
      ],
    ],
  },
  {
    code: '[Column]',
    expected: ['Expression', ['Column', ['OpenBracket', '['], ['CloseBracket', ']']]],
  },
  {
    code: '[Column 1]',
    expected: ['Expression', ['Column', ['OpenBracket', '['], ['CloseBracket', ']']]],
  },
  {
    code: 'number(1)',
    expected: [
      'Expression',
      ['Function', ['OpenParen', '('], ['Number', '1'], ['CloseParen', ')']],
    ],
  },
  {
    code: 'text_length([Column 1])',
    expected: [
      'Expression',
      [
        'Function',
        ['OpenParen', '('],
        ['Column', ['OpenBracket', '['], ['CloseBracket', ']']],
        ['CloseParen', ')'],
      ],
    ],
  },
  {
    code: 'without_parens',
    expected: ['Expression', ['Function', ['⚠', '']]],
  },
  {
    code: 'open_paren_only(',
    expected: ['Expression', ['Function', ['OpenParen', '('], ['⚠', '']]],
  },
  {
    code: 'unclosed_column_in_function([Column 1)',
    expected: [
      'Expression',
      ['Function', ['OpenParen', '('], ['Column', ['OpenBracket', '['], ['⚠', '']], ['⚠', '']],
    ],
  },
  {
    code: '[Column 1]+|',
    expected: [
      'Expression',
      [
        'BinOpApp',
        ['Column', ['OpenBracket', '['], ['CloseBracket', ']']],
        ['ArithOp', '+'],
        ['⚠', '|'],
      ],
    ],
  },
  {
    code: '[Column 1] + |',
    expected: [
      'Expression',
      [
        'BinOpApp',
        ['Column', ['OpenBracket', '['], ['CloseBracket', ']']],
        ['ArithOp', '+'],
        ['⚠', '|'],
      ],
    ],
  },
  // FIXME: The parser should probably treat this as a binop.
  {
    code: '| + [Column 1]',
    expected: ['Expression', ['Date', ['⚠', ''], ['⚠', '']]],
  },
  {
    code: '|+[Column 1]',
    expected: ['Expression', ['Date', ['⚠', ''], ['⚠', '']]],
  },
  {
    code: '[',
    expected: ['Expression', ['Column', ['OpenBracket', '['], ['⚠', '']]],
  },
  {
    code: 'not',
    expected: ['Expression', ['PrefixOpApp', ['ArithOp', ['NOT', 'not']], ['⚠', '']]],
  },
  {
    code: '!',
    expected: ['Expression', ['PrefixOpApp', ['ArithOp', '!'], ['⚠', '']]],
  },
  {
    code: '(1)',
    expected: ['Expression', ['OpenParen', '('], ['Number', '1'], ['CloseParen', ')']],
  },
  {
    code: '!1',
    expected: ['Expression', ['PrefixOpApp', ['ArithOp', '!'], ['Number', '1']]],
  },
  {
    code: '(!1)',
    expected: [
      'Expression',
      ['OpenParen', '('],
      ['PrefixOpApp', ['ArithOp', '!'], ['Number', '1']],
      ['CloseParen', ')'],
    ],
  },
  {
    code: '',
    expected: ['Expression', ['⚠', '']],
  },
  // Why is this empty expression a number expression (with an empty content error), instead of a
  // missing-expression error?
  {
    code: '()',
    expected: ['Expression', ['OpenParen', '('], ['Number', ['⚠', '']], ['CloseParen', ')']],
  },
  {
    code: '! ()',
    expected: [
      'Expression',
      [
        'PrefixOpApp',
        ['ArithOp', '!'],
        ['OpenParen', '('],
        ['Number', ['⚠', '']],
        ['CloseParen', ')'],
      ],
    ],
  },
  {
    code: '[',
    expected: ['Expression', ['Column', ['OpenBracket', '['], ['⚠', '']]],
  },
])('Syntax tree', ({ code, expected }) => {
  expect(debugTree(parser.parse(code), code)).toEqual(expected)
})
