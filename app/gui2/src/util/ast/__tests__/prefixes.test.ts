import { Ast } from '@/util/ast/abstract'
import { Prefixes } from '@/util/ast/prefixes'
import { expect, test } from 'vitest'

test.each([
  {
    prefixes: {
      a: 'a + __',
    },
    modifications: {
      a: [],
    },
    source: 'b',
    target: 'a + b',
  },
  {
    prefixes: {
      a: 'a + __ + c',
    },
    modifications: {
      a: [],
    },
    source: 'd',
    target: 'a + d + c',
  },
  {
    prefixes: {
      a: '__+e +   __',
    },
    modifications: {
      a: ['d'],
    },
    source: 'f',
    target: 'd+e +   f',
  },
  {
    prefixes: {
      enable:
        'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output __ <| __',
    },
    modifications: {
      enable: ["'foo'"],
    },
    source: 'a + b',
    target:
      "Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output 'foo' <| a + b",
  },
  {
    prefixes: {
      a: '__+e +   __',
    },
    modifications: {
      a: undefined,
    },
    source: 'd+e +   f',
    target: 'f',
  },
  {
    prefixes: {
      a: '__+e +   __',
    },
    modifications: {
      a: undefined,
    },
    source: 'd + e + f',
    target: 'f',
  },
])('modify', ({ prefixes: lines, modifications, source, target }) => {
  const prefixes = Prefixes.FromLines(lines as any)
  const sourceAst = Ast.parseLine(source)
  const edit = sourceAst.module.edit()
  const modificationAsts = Object.fromEntries(
    Object.entries(modifications).map(([k, v]) => [
      k,
      v ? Array.from(v, (mod) => Ast.parse(mod, edit)) : undefined,
    ]),
  )
  expect(prefixes.modify(edit, sourceAst, modificationAsts).code(edit)).toBe(target)
})
