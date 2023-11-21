import { Tree } from '@/generated/ast'
import { AstExtended } from '@/util/ast'
import { ArgumentApplication, ArgumentPlaceholder } from '@/util/callTree'
import { isSome } from '@/util/opt'
import { isInstance } from '@/util/predicates'
import type { SuggestionEntryArgument } from 'shared/languageServerTypes/suggestions'
import { IdMap } from 'shared/yjsModel'
import { assert, expect, test } from 'vitest'

const knownArguments: SuggestionEntryArgument[] = [
  { name: 'a', type: 'Any', isSuspended: false, hasDefault: false },
  { name: 'b', type: 'Any', isSuspended: false, hasDefault: false },
  { name: 'c', type: 'Any', isSuspended: false, hasDefault: false },
  { name: 'd', type: 'Any', isSuspended: false, hasDefault: false },
]

function testArgs(paddedExpression: string, pattern: string) {
  const expression = paddedExpression.trim()
  const notAppliedArguments = pattern
    .split(' ')
    .map((p) => (p.startsWith('?') ? knownArguments.findIndex((k) => p.endsWith(k.name)) : null))
    .filter(isSome)

  test(`argument list: ${paddedExpression} ${pattern}`, () => {
    const parsedBlock = AstExtended.parse(expression, IdMap.Mock())
    assert(parsedBlock.isTree(Tree.Type.BodyBlock)) // necessary for type inference
    const ast = parsedBlock.map((t) => {
      const first = t.statements.next()
      assert(first.done === false)
      assert(first.value.expression != null)
      return first.value.expression
    })

    const call = ArgumentApplication.FromAstWithInfo(ast, knownArguments, notAppliedArguments)
    assert(isInstance(ArgumentApplication, call))
    expect(printArgPattern(call)).toEqual(pattern)
  })
}

function printArgPattern(application: ArgumentApplication | AstExtended<Tree>) {
  const parts: string[] = []
  let current: ArgumentApplication | AstExtended<Tree> = application
  while (isInstance(ArgumentApplication, current)) {
    const sigil = isInstance(ArgumentPlaceholder, current.argument)
      ? '?'
      : current.appTree?.isTree(Tree.Type.NamedApp)
      ? '='
      : '@'
    parts.push(sigil + (current.argument.info?.name ?? '_'))
    current = current.target
  }
  return parts.reverse().join(' ')
}

testArgs('func              ', '?a ?b ?c ?d')
testArgs('func a=x c=x      ', '=a ?b =c ?d')
testArgs('func a=x x c=x    ', '=a @b =c ?d')
testArgs('func a=x d=x      ', '=a ?b ?c =d')
testArgs('func a=x d=x b=x  ', '=a =d =b ?c')
testArgs('func a=x d=x c=x  ', '=a =d ?b =c')
testArgs('func a=x c=x d=x  ', '=a ?b =c =d')
testArgs('func b=x          ', '?a =b ?c ?d')
testArgs('func b=x c=x      ', '?a =b =c ?d')
testArgs('func b=x x x      ', '=b @a @c ?d')
testArgs('func c=x b=x x    ', '=c =b @a ?d')
testArgs('func d=x          ', '?a ?b ?c =d')
testArgs('func d=x a c=x    ', '=d @a ?b =c')
testArgs('func d=x x        ', '=d @a ?b ?c')
testArgs('func d=x x        ', '=d @a ?b ?c')
testArgs('func d=x x x      ', '=d @a @b ?c')
testArgs('func d=x x x x    ', '=d @a @b @c')
testArgs('func x            ', '@a ?b ?c ?d')
testArgs('func x b=x c=x    ', '@a =b =c ?d')
testArgs('func x b=x x      ', '@a =b @c ?d')
testArgs('func x d=x        ', '@a ?b ?c =d')
testArgs('func x x          ', '@a @b ?c ?d')
testArgs('func x x x        ', '@a @b @c ?d')
testArgs('func x x x x      ', '@a @b @c @d')
testArgs('func a=x x m=x    ', '=a @b =m ?c ?d')
