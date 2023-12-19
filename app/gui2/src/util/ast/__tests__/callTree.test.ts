import { SuggestionKind, type SuggestionEntry } from '@/stores/suggestionDatabase/entry'
import { Ast } from '@/util/ast'
import { ArgumentApplication, ArgumentPlaceholder } from '@/util/callTree'
import { isSome } from '@/util/data/opt'
import { type Identifier, type QualifiedName } from '@/util/qualifiedName'
import type { MethodCall } from 'shared/languageServerTypes'
import { assert, expect, test } from 'vitest'

const mockSuggestion: SuggestionEntry = {
  kind: SuggestionKind.Method,
  name: 'func' as Identifier,
  definedIn: 'Foo.Bar' as QualifiedName,
  selfType: 'Foo.Bar',
  returnType: 'Any',
  arguments: [
    { name: 'self', type: 'Any', isSuspended: false, hasDefault: false },
    { name: 'a', type: 'Any', isSuspended: false, hasDefault: false },
    { name: 'b', type: 'Any', isSuspended: false, hasDefault: false },
    { name: 'c', type: 'Any', isSuspended: false, hasDefault: false },
    { name: 'd', type: 'Any', isSuspended: false, hasDefault: false },
  ],
  documentation: [],
  isPrivate: false,
  isUnstable: false,
  aliases: [],
}

function testArgs(paddedExpression: string, pattern: string) {
  const expression = paddedExpression.trim()
  const notAppliedArguments = pattern
    .split(' ')
    .map((p) =>
      p.startsWith('?') ? mockSuggestion.arguments.findIndex((k) => p.slice(1) === k.name) : null,
    )
    .filter(isSome)

  test(`argument list: ${paddedExpression} ${pattern}`, () => {
    const parsedBlock = Ast.parse(expression)
    assert(parsedBlock instanceof Ast.BodyBlock) // necessary for type inference
    const expressions = Array.from(parsedBlock.expressions())
    const first = expressions[0]
    assert(first !== undefined)
    const ast = first

    const methodCall: MethodCall = {
      methodPointer: {
        name: 'func',
        definedOnType: 'Foo.Bar',
        module: 'Foo.Bar',
      },
      notAppliedArguments,
    }

    const funcMethodCall: MethodCall = {
      methodPointer: {
        name: 'func',
        definedOnType: 'Foo.Bar',
        module: 'Foo.Bar',
      },
      notAppliedArguments: [1, 2, 3, 4],
    }

    const interpreted = ArgumentApplication.Interpret(ast, false)
    const call = ArgumentApplication.FromInterpretedWithInfo(
      interpreted,
      funcMethodCall,
      methodCall,
      mockSuggestion,
    )
    assert(call instanceof ArgumentApplication)
    expect(printArgPattern(call)).toEqual(pattern)
  })
}

function printArgPattern(application: ArgumentApplication | Ast.Ast) {
  const parts: string[] = []
  let current: ArgumentApplication['target'] = application
  while (current instanceof ArgumentApplication) {
    const sigil =
      current.argument instanceof ArgumentPlaceholder
        ? '?'
        : current.appTree instanceof Ast.App && current.appTree.argumentName
        ? '='
        : '@'
    const argInfo = 'info' in current.argument ? current.argument.info : undefined
    parts.push(sigil + (argInfo?.name ?? '_'))
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
