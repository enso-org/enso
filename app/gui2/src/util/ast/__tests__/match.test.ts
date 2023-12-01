import { AstExtended } from '@/util/ast'
import { extractMatches, isMatch } from '@/util/ast/match'
import { expect, test } from 'vitest'

test.each([
  { target: '1 + 1', pattern: '1  +  1', extracted: [] },
  { target: '1 + 2', pattern: '1 + __', extracted: ['2'] },
  { target: '1 + 2', pattern: '__ + 2', extracted: ['1'] },
  { target: '1 + 2', pattern: '__ + __', extracted: ['1', '2'] },
  { target: '1', pattern: '__', extracted: ['1'] },
  { target: '1', pattern: '(__)' },
  { target: '("a")', pattern: '(__)', extracted: ['"a"'] },
  { target: '[1, "a", True]', pattern: '[1, "a", True]', extracted: [] },
  { target: '[1,      "a", True]', pattern: '[__, "a",      __]', extracted: ['1', 'True'] },
  { target: '[1, "a", True]', pattern: '[1, "a", False]' },
  { target: '[1, "a", True]', pattern: '[1, "a", True, 2]' },
  { target: '[1, "a", True]', pattern: '[1, "a", True, __]' },
  { target: '(1)', pattern: '1' },
  { target: '(1)', pattern: '__', extracted: ['(1)'] }, // True because `__` matches any expression.
  { target: '1 + 1', pattern: '1 + 2' },
  { target: '1 + 1', pattern: '"1" + 1' },
  { target: '1 + 1', pattern: '1 + "1"' },
  { target: '1 + 1 + 1', pattern: '(1 + 1) + 1' },
  { target: '1 + 1 + 1', pattern: '1 + (1 + 1)' },
  { target: '(1 + 1) + 1', pattern: '1 + 1 + 1' },
  { target: '1 + (1 + 1)', pattern: '1 + 1 + 1' },
  {
    target:
      'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output "current_context_name" <| node1.fn',
    pattern:
      'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output "current_context_name" <| __',
    extracted: ['node1.fn'],
  },
  {
    target:
      'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output "current_context_name" <| node1.fn',
    pattern:
      'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output __ <| __',
    extracted: ['"current_context_name"', 'node1.fn'],
  },
  {
    target:
      'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output "current_context_name" <| node1.fn',
    pattern: 'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output __',
  },
  {
    target:
      'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output "current_context_name" <| node1.fn',
    pattern:
      'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output "current_context#name" <| node1.fn',
  },
  {
    target:
      'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output "current_context_name" <| node1.fn',
    pattern:
      'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output "current_context_name" <| node2.fn',
  },
])('`isMatch`', ({ target, pattern, extracted }) => {
  const targetAst = AstExtended.parse(target)
  const patternAst = AstExtended.parse(pattern)
  expect(
    isMatch(targetAst, patternAst),
    `'${target}' has CST ${extracted != null ? '' : 'not '}matching '${pattern}'`,
  ).toBe(extracted != null)
  expect(
    extractMatches(targetAst, patternAst)?.map((match) => match.repr()),
    extracted != null
      ? `'${target}' matches '${pattern}' with '__' corresponding to '${extracted}'`
      : `'${target}' does not match '${pattern}'`,
  ).toStrictEqual(extracted)
})
