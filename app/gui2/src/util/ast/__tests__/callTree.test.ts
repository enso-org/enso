import * as widgetCfg from '@/providers/widgetRegistry/configuration'
import { makeArgument, makeMethod, makeModuleMethod } from '@/stores/suggestionDatabase/entry'
import { Ast } from '@/util/ast'
import {
  ArgumentApplication,
  ArgumentAst,
  ArgumentPlaceholder,
  interpretCall,
} from '@/util/callTree'
import { isSome } from '@/util/data/opt'
import type { MethodCall } from 'shared/languageServerTypes'
import { assert, expect, test } from 'vitest'

const prefixFixture = {
  mockSuggestion: {
    ...makeModuleMethod('local.Foo.Bar.func'),
    arguments: ['self', 'a', 'b', 'c', 'd'].map((name) => makeArgument(name)),
  },
  argsParameters: new Map<string, widgetCfg.WidgetConfiguration & widgetCfg.WithDisplay>([
    ['a', { kind: 'Multi_Choice', display: widgetCfg.DisplayMode.Always }],
    ['b', { kind: 'Code_Input', display: widgetCfg.DisplayMode.Always }],
    ['c', { kind: 'Boolean_Input', display: widgetCfg.DisplayMode.Always }],
  ]),
  methodPointer: {
    name: 'func',
    definedOnType: 'Foo.Bar',
    module: 'local.Foo.Bar',
  },
}

const infixFixture = {
  mockSuggestion: {
    ...makeMethod('local.Foo.Bar.Buz.+'),
    arguments: ['lhs', 'rhs'].map((name) => makeArgument(name)),
  },
  argsParameters: new Map<string, widgetCfg.WidgetConfiguration & widgetCfg.WithDisplay>([
    ['lhs', { kind: 'Multi_Choice', display: widgetCfg.DisplayMode.Always }],
    ['rhs', { kind: 'Code_Input', display: widgetCfg.DisplayMode.Always }],
  ]),
  methodPointer: {
    name: '+',
    definedOnType: 'local.Foo.Bar.Buz',
    module: 'local.Foo.Bar',
  },
}

interface TestData {
  expression: string
  expectedPattern: string
  fixture: typeof prefixFixture | typeof infixFixture
}

test.each`
  expression              | expectedPattern     | fixture
  ${'func              '} | ${'?a ?b ?c ?d'}    | ${prefixFixture}
  ${'func a=x c=x      '} | ${'=a ?b =c ?d'}    | ${prefixFixture}
  ${'func a=x x c=x    '} | ${'=a @b =c ?d'}    | ${prefixFixture}
  ${'func a=x d=x      '} | ${'=a ?b ?c =d'}    | ${prefixFixture}
  ${'func a=x d=x b=x  '} | ${'=a =d =b ?c'}    | ${prefixFixture}
  ${'func a=x d=x c=x  '} | ${'=a ?b =d =c'}    | ${prefixFixture}
  ${'func a=x c=x d=x  '} | ${'=a ?b =c =d'}    | ${prefixFixture}
  ${'func b=x          '} | ${'?a =b ?c ?d'}    | ${prefixFixture}
  ${'func b=x c=x      '} | ${'?a =b =c ?d'}    | ${prefixFixture}
  ${'func b=x x x      '} | ${'=b @a @c ?d'}    | ${prefixFixture}
  ${'func c=x b=x x    '} | ${'=c =b @a ?d'}    | ${prefixFixture}
  ${'func d=x          '} | ${'?a ?b ?c =d'}    | ${prefixFixture}
  ${'func d=x a c=x    '} | ${'=d @a ?b =c'}    | ${prefixFixture}
  ${'func d=x x        '} | ${'=d @a ?b ?c'}    | ${prefixFixture}
  ${'func d=x x        '} | ${'=d @a ?b ?c'}    | ${prefixFixture}
  ${'func d=x x x      '} | ${'=d @a @b ?c'}    | ${prefixFixture}
  ${'func d=x x x x    '} | ${'=d @a @b @c'}    | ${prefixFixture}
  ${'func x            '} | ${'@a ?b ?c ?d'}    | ${prefixFixture}
  ${'func x b=x c=x    '} | ${'@a =b =c ?d'}    | ${prefixFixture}
  ${'func x b=x x      '} | ${'@a =b @c ?d'}    | ${prefixFixture}
  ${'func x d=x        '} | ${'@a ?b ?c =d'}    | ${prefixFixture}
  ${'func x x          '} | ${'@a @b ?c ?d'}    | ${prefixFixture}
  ${'func x x x        '} | ${'@a @b @c ?d'}    | ${prefixFixture}
  ${'func x x x x      '} | ${'@a @b @c @d'}    | ${prefixFixture}
  ${'func a=x x m=x    '} | ${'=a @b ?c ?d =m'} | ${prefixFixture}
  ${'x + y'}              | ${'@lhs @rhs'}      | ${infixFixture}
  ${'x +'}                | ${'@lhs ?rhs'}      | ${infixFixture}
`(
  "Creating argument application's info: $expression $expectedPattern",
  ({
    expression,
    expectedPattern,
    fixture: { mockSuggestion, argsParameters, methodPointer },
  }: TestData) => {
    const expectedArgs = expectedPattern.split(' ')
    const notAppliedArguments = expectedArgs
      .map((p: string) =>
        p.startsWith('?') ? mockSuggestion.arguments.findIndex((k) => p.slice(1) === k.name) : null,
      )
      .filter(isSome)

    const ast = Ast.parse(expression.trim())

    const methodCall: MethodCall = {
      methodPointer,
      notAppliedArguments,
    }

    const funcMethodCall: MethodCall = {
      methodPointer,
      notAppliedArguments: Array.from(expectedArgs, (_, i) => i + 1),
    }

    const configuration: widgetCfg.FunctionCall = {
      kind: 'FunctionCall',
      parameters: argsParameters,
    }

    const interpreted = interpretCall(ast, true)
    const call = ArgumentApplication.FromInterpretedWithInfo(interpreted, {
      appMethodCall: methodCall,
      noArgsCall: funcMethodCall,
      suggestion: mockSuggestion,
      widgetCfg: configuration,
    })
    assert(call instanceof ArgumentApplication)
    expect(printArgPattern(call)).toEqual(expectedPattern)
    checkArgsConfig(call, argsParameters)
  },
)

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
    parts.push(sigil + (current.argument.argInfo?.name ?? '_'))
    current = current.target
  }
  if (current instanceof ArgumentPlaceholder) parts.push(`?${current.argInfo.name}`)
  if (current instanceof ArgumentAst) parts.push(`@${current.argInfo?.name}`)
  return parts.reverse().join(' ')
}

function checkArgsConfig(
  application: ArgumentApplication | Ast.Ast,
  argConfig: Map<string, widgetCfg.WidgetConfiguration | widgetCfg.WithDisplay>,
) {
  let current: ArgumentApplication['target'] = application
  while (current instanceof ArgumentApplication) {
    const argName = current.argument.argInfo?.name
    const expected = argName ? argConfig.get(argName) : undefined
    expect(current.argument.dynamicConfig).toEqual(expected)
    current = current.target
  }
}
