import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import { expect, test } from 'vitest'
import { MutableModule } from '../abstract'

test.each([
  { target: 'a.b', pattern: '__', extracted: ['a.b'] },
  { target: 'a.b', pattern: 'a.__', extracted: ['b'] },
  { target: 'a.b', pattern: '__.b', extracted: ['a'] },
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
  {
    target:
      'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output "current_context_name" <| a + b',
    pattern:
      'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output __ <| __',
    extracted: ['"current_context_name"', 'a + b'],
  },
  {
    target:
      "Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output 'current_context_name' <| a + b",
    pattern:
      'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output __ <| __',
    extracted: ["'current_context_name'", 'a + b'],
  },
  {
    target:
      "Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output 'current_context_name' <| a + b",
    pattern: 'Standard.Base.Runtime.__ Standard.Base.Runtime.Context.Output __ <| __',
    extracted: ['with_enabled_context', "'current_context_name'", 'a + b'],
  },
])('`isMatch` and `extractMatches`', ({ target, pattern, extracted }) => {
  const targetAst = Ast.parseExpression(target)
  const module = targetAst.module
  const patternAst = Pattern.parse(pattern)
  expect(
    patternAst.match(targetAst) !== undefined,
    `'${target}' has CST ${extracted != null ? '' : 'not '}matching '${pattern}'`,
  ).toBe(extracted != null)
  expect(
    patternAst.match(targetAst)?.map((match) => module.get(match)?.code()),
    extracted != null
      ? `'${target}' matches '${pattern}' with '__'s corresponding to ${JSON.stringify(extracted)
          .slice(1, -1)
          .replace(/"/g, "'")}`
      : `'${target}' does not match '${pattern}'`,
  ).toStrictEqual(extracted)
})

test.each([
  { template: 'a __ c', source: 'b', result: 'a b c' },
  { template: 'a . __ . c', source: 'b', result: 'a . b . c' },
])('instantiate', ({ template, source, result }) => {
  const pattern = Pattern.parse(template)
  const edit = MutableModule.Transient()
  const intron = Ast.parse(source, edit)
  const instantiated = pattern.instantiate(edit, [intron.exprId])
  expect(instantiated.code(edit)).toBe(result)
})
