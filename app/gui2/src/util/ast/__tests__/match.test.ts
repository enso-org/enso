import { AstExtended } from '@/util/ast'
import { isMatch } from '@/util/ast/match'
import { expect, test } from 'vitest'

test.each([
  { target: '1 + 1', pattern: '1 + 1' },
  { target: '1 + 1', pattern: '1 + __' },
  { target: '1 + 1', pattern: '__ + 1' },
  { target: '1', pattern: '__' },
  { target: '1', pattern: '(__)', matches: false },
  { target: '("a")', pattern: '(__)' },
  { target: '[1, "a", True]', pattern: '[1, "a", True]' },
  { target: '[1, "a", True]', pattern: '[__, "a", __]' },
  { target: '[1, "a", True]', pattern: '[1, "a", False]', matches: false },
  { target: '[1, "a", True]', pattern: '[1, "a", True, 2]', matches: false },
  { target: '[1, "a", True]', pattern: '[1, "a", True, __]', matches: false },
  { target: '(1)', pattern: '1', matches: false },
  { target: '(1)', pattern: '__' }, // True because `__` matches any expression.
  { target: '1 + 1', pattern: '1 + 2', matches: false },
  { target: '1 + 1', pattern: '"1" + 1', matches: false },
  { target: '1 + 1', pattern: '1 + "1"', matches: false },
  { target: '1 + 1 + 1', pattern: '(1 + 1) + 1', matches: false },
  { target: '1 + 1 + 1', pattern: '1 + (1 + 1)', matches: false },
  { target: '(1 + 1) + 1', pattern: '1 + 1 + 1', matches: false },
  { target: '1 + (1 + 1)', pattern: '1 + 1 + 1', matches: false },
  {
    target:
      'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output "current_context_name" <| node1.fn',
    pattern:
      'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output "current_context_name" <| __',
  },
  {
    target:
      'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output "current_context_name" <| node1.fn',
    pattern:
      'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output __ <| __',
  },
  {
    target:
      'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output "current_context_name" <| node1.fn',
    pattern: 'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output __',
    matches: false,
  },
  {
    target:
      'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output "current_context_name" <| node1.fn',
    pattern:
      'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output "current_context#name" <| node1.fn',
    matches: false,
  },
  {
    target:
      'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output "current_context_name" <| node1.fn',
    pattern:
      'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output "current_context_name" <| node2.fn',
    matches: false,
  },
])('`isMatch`', ({ target, pattern, matches = true }) => {
  expect(
    isMatch(AstExtended.parse(target), AstExtended.parse(pattern)),
    `'${target}' has CST ${matches ? '' : 'not '}matching '${pattern}'`,
  ).toBe(matches)
})
