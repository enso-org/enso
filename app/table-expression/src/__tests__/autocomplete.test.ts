import { EditorState } from '@codemirror/state'
import { expect, test } from 'vitest'
import { tableExpression } from '..'
import { completionTypeAt } from '../autocomplete'

function completionTypeCase(source: string) {
  const anchor = source.indexOf('|')
  if (anchor === -1) throw new Error('No selection found in test input')
  const secondAnchor = source.indexOf('|', anchor + 1)
  const pos = secondAnchor > 0 ? secondAnchor - 1 : anchor
  const doc = source.replaceAll('|', '')
  const state = EditorState.create({
    doc,
    extensions: [tableExpression({ methods: () => [] })],
  })
  return { completion: completionTypeAt(pos, state), anchor }
}

test.each([
  {
    source: '|',
    auto: true,
    insertParen: true,
  },
  {
    source: '|a_function(1, 2, 3)',
    auto: false,
    insertParen: false,
  },
  {
    source: '|a_|function(1, 2, 3)',
    auto: false,
    insertParen: false,
  },
  {
    source: '|a_func|tion(1, 2, 3)',
    auto: false,
    insertParen: false,
  },
  {
    source: '|a_function|(1, 2, 3)',
    auto: true,
    insertParen: false,
  },
  {
    source: '|without_parens',
    auto: false,
    insertParen: true,
  },
  {
    source: '|without|_parens',
    auto: false,
    insertParen: true,
  },
  {
    source: '|without_parens|',
    auto: true,
    insertParen: true,
  },
])('Function name completion: $source', ({ source, auto, insertParen }) => {
  const { completion, anchor } = completionTypeCase(source)
  expect(completion).toStrictEqual({ type: 'functionName', pos: anchor, auto, insertParen })
})

test.each([
  'a_function(|1, 2, 3)',
  'a_function(1|, 2, 3)',
  'a_function(1,| 2, 3)',
  'a_function(1, |2, 3)',
  'a_function(1, 2, 3|)',
])('Function info completion: %s', (source) => {
  const { completion } = completionTypeCase(source)
  expect(completion).toStrictEqual({ type: 'functionInfo', functionName: 'a_function' })
})

test.each(['a_function(1, 2, 3)|'])('Non-completable position', (source) =>
  expect(completionTypeCase(source).completion).toBeNull(),
)
