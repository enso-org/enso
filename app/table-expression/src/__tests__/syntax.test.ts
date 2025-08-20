import { type TreeCursor } from '@lezer/common'
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
    expected: ['Expression', ['Number', '1'], ['ArithOp', '+'], ['Number', '1']],
  },
  {
    code: '[Column]',
    expected: ['Expression', ['Column', ['SquareBracket', '['], ['SquareBracket', ']']]],
  },
  {
    code: '[Column 1]',
    expected: ['Expression', ['Column', ['SquareBracket', '['], ['SquareBracket', ']']]],
  },
  {
    code: 'number(1)',
    expected: ['Expression', ['Function', ['Paren', '('], ['Number', '1'], ['Paren', ')']]],
  },
  {
    code: 'text_length([Column 1])',
    expected: [
      'Expression',
      [
        'Function',
        ['Paren', '('],
        ['Column', ['SquareBracket', '['], ['SquareBracket', ']']],
        ['Paren', ')'],
      ],
    ],
  },
  {
    code: 'without_parens',
    expected: ['Expression', ['Function', ['⚠', '']]],
  },
  {
    code: 'open_paren_only(',
    expected: ['Expression', ['Function', ['Paren', '('], ['⚠', '']]],
  },
  {
    code: 'unclosed_column_in_function([Column 1)',
    expected: [
      'Expression',
      ['Function', ['Paren', '('], ['Column', ['SquareBracket', '['], ['⚠', '']], ['⚠', '']],
    ],
  },
])('Syntax tree', ({ code, expected }) => {
  expect(debugTree(parser.parse(code), code)).toEqual(expected)
})
