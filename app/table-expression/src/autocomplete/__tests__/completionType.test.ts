import { EditorState } from '@codemirror/state'
import { expect, test } from 'vitest'
import { completionTypeAt } from '..'
import { tableExpression } from '../..'

function completionTypeCase(source: string) {
  const anchor = source.indexOf('|')
  if (anchor === -1) throw new Error('No selection found in test input')
  const secondAnchor = source.indexOf('|', anchor + 1)
  const pos = secondAnchor > 0 ? secondAnchor - 1 : anchor
  const doc = source.replaceAll('|', '')
  const state = EditorState.create({
    doc,
    extensions: tableExpression(),
  })
  return { completion: completionTypeAt(pos, state), anchor }
}

test.each([
  {
    source: '|a_function(1, 2, 3)',
    auto: false,
    insertDelim: false,
  },
  {
    source: '|a_|function(1, 2, 3)',
    auto: false,
    insertDelim: false,
  },
  {
    source: '|a_func|tion(1, 2, 3)',
    auto: false,
    insertDelim: false,
  },
  {
    source: '|a_function|(1, 2, 3)',
    auto: true,
    insertDelim: false,
  },
  {
    source: '|without_parens',
    auto: false,
    insertDelim: true,
  },
  {
    source: '|without|_parens',
    auto: false,
    insertDelim: true,
  },
  {
    source: '|without_parens|',
    auto: true,
    insertDelim: true,
  },
])('Function name completion: $source', ({ source, auto, insertDelim }) => {
  const { completion, anchor } = completionTypeCase(source)
  expect(completion).toStrictEqual({ type: 'functionName', pos: anchor, auto, insertDelim })
})

test.each([
  'a_function(|1, 2, 3)',
  'a_function(1|, 2, 3)',
  'a_function(1,| 2, 3)',
  'a_function(1, |2, 3)',
  'a_function(1, 2, 3|)',
])('Function info completion: %s', (source) => {
  const { completion } = completionTypeCase(source)
  expect(completion).toStrictEqual({ type: 'functionInfo', pos: 0, functionName: 'a_function' })
})

test.each([
  { source: '[|Column 1]', auto: false, insertDelim: false },
  { source: '[|Column| 1]', auto: false, insertDelim: false },
  { source: '[|Column 1|]', auto: true, insertDelim: false },
  { source: 'a_function([|Column 1])', auto: false, insertDelim: false },
  { source: 'a_function([|Column| 1])', auto: false, insertDelim: false },
  { source: 'a_function([|Column 1|])', auto: true, insertDelim: false },
  { source: '[|Column 1', auto: false, insertDelim: true },
  { source: '[|Column| 1', auto: false, insertDelim: true },
  { source: '[|Column 1|', auto: true, insertDelim: true },
  { source: 'a_function([|Column 1', auto: false, insertDelim: true },
  { source: 'a_function([|Column| 1', auto: false, insertDelim: true },
  { source: 'a_function([|Column 1|', auto: true, insertDelim: true },
  { source: 'a_function([|Column 1)', auto: false, insertDelim: true },
  { source: 'a_function([|Column| 1)', auto: false, insertDelim: true },
  // NOTE: `auto: false` here because the parser's error recovery treats the close-paren as part of
  // the column name. This is hard to avoid because parens are legal in column names.
  { source: 'a_function([|Column 1|)', auto: false, insertDelim: true },
  // A bit surprising, for the same reason as the previous case.
  { source: 'a_function([|Column 1)|', auto: true, insertDelim: true },
  { source: '[|', auto: true, insertDelim: true },
])('Column completion: $source', ({ source, auto, insertDelim }) => {
  const { completion, anchor } = completionTypeCase(source)
  expect(completion).toStrictEqual({ type: 'columnName', pos: anchor, auto, insertDelim })
})

test.each(['|', '[Column 1] + |', '!|', '! |', '!(|)', '! (|)', 'not |', 'not(|)', 'not (|)'])(
  'Any-value completion: %s',
  (source) => {
    const { completion } = completionTypeCase(source)
    expect(completion).toStrictEqual({ type: 'value' })
  },
)

test.each([
  { source: '[Column 1]|', auto: false, insertDelim: false },
  { source: '[Column 1] |', auto: true, insertDelim: true },
  { source: 'a_function([Column 1])|', auto: false, insertDelim: false },
  { source: 'a_function([Column 1]) |', auto: true, insertDelim: true },
])('Binop completion: $source', ({ source, auto, insertDelim }) => {
  const { completion, anchor: pos } = completionTypeCase(source)
  expect(completion).toStrictEqual({ type: 'binop', pos, auto, insertDelim })
})
