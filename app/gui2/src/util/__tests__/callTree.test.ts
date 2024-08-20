import * as widgetCfg from '@/providers/widgetRegistry/configuration'
import { GraphDb } from '@/stores/graph/graphDatabase'
import { ComputedValueRegistry, type ExpressionInfo } from '@/stores/project/computedValueRegistry'
import { SuggestionDb } from '@/stores/suggestionDatabase'
import {
  makeArgument,
  makeConstructor,
  makeMethod,
  makeModule,
  makeModuleMethod,
  makeType,
  type SuggestionEntry,
} from '@/stores/suggestionDatabase/entry'
import { Ast } from '@/util/ast'
import type { AstId } from '@/util/ast/abstract'
import {
  ArgumentApplication,
  ArgumentAst,
  ArgumentPlaceholder,
  getMethodCallInfoRecursively,
  interpretCall,
} from '@/util/callTree'
import { fail } from 'assert'
import { assert, expect, test } from 'vitest'
import type { ExpressionUpdatePayload, MethodCall } from 'ydoc-shared/languageServerTypes'
import { assertEqual, assertNotEqual } from 'ydoc-shared/util/assert'

const prefixFixture = {
  allowInfix: false,
  mockSuggestion: {
    ...makeModuleMethod('local.Foo.Bar.func'),
    arguments: ['self', 'a', 'b', 'c', 'd'].map((name) => makeArgument(name)),
  },
  argsParameters: new Map<string, widgetCfg.WidgetConfiguration & widgetCfg.WithDisplay>([
    [
      'a',
      { kind: 'Multiple_Choice', display: widgetCfg.DisplayMode.Always, label: null, values: [] },
    ],
    ['b', { kind: 'Code_Input', display: widgetCfg.DisplayMode.Always }],
    ['c', { kind: 'Boolean_Input', display: widgetCfg.DisplayMode.Always }],
  ]),
}

const infixFixture = {
  allowInfix: true,
  mockSuggestion: {
    ...makeMethod('local.Foo.Bar.Buz.+'),
    arguments: ['lhs', 'rhs'].map((name) => makeArgument(name)),
  },
  argsParameters: new Map<string, widgetCfg.WidgetConfiguration & widgetCfg.WithDisplay>([
    [
      'lhs',
      { kind: 'Multiple_Choice', display: widgetCfg.DisplayMode.Always, label: null, values: [] },
    ],
    ['rhs', { kind: 'Code_Input', display: widgetCfg.DisplayMode.Always }],
  ]),
}

interface TestData {
  expression: string
  expectedPattern: string
  fixture: typeof prefixFixture | typeof infixFixture
}

test.each`
  expression                 | expectedPattern        | fixture
  ${'func                '}  | ${'?self ?a ?b ?c ?d'} | ${prefixFixture}
  ${'a.func              '}  | ${'?a ?b ?c ?d'}       | ${prefixFixture}
  ${'a.func a=x c=x      '}  | ${'=a ?b =c ?d'}       | ${prefixFixture}
  ${'a.func a=x x c=x    '}  | ${'=a @b =c ?d'}       | ${prefixFixture}
  ${'a.func a=x d=x      '}  | ${'=a ?b ?c =d'}       | ${prefixFixture}
  ${'a.func a=x d=x b=x  '}  | ${'=a =d =b ?c'}       | ${prefixFixture}
  ${'a.func a=x d=x c=x  '}  | ${'=a ?b =d =c'}       | ${prefixFixture}
  ${'func a=x d=x c=x  '}    | ${'?self =a ?b =d =c'} | ${prefixFixture}
  ${'func self=x d=x c=x  '} | ${'=self ?a ?b =d =c'} | ${prefixFixture}
  ${'a.func a=x c=x d=x  '}  | ${'=a ?b =c =d'}       | ${prefixFixture}
  ${'a.func b=x          '}  | ${'?a =b ?c ?d'}       | ${prefixFixture}
  ${'a.func b=x c=x      '}  | ${'?a =b =c ?d'}       | ${prefixFixture}
  ${'a.func b=x x x      '}  | ${'=b @a @c ?d'}       | ${prefixFixture}
  ${'a.func c=x b=x x    '}  | ${'=c =b @a ?d'}       | ${prefixFixture}
  ${'a.func d=x          '}  | ${'?a ?b ?c =d'}       | ${prefixFixture}
  ${'a.func d=x a c=x    '}  | ${'=d @a ?b =c'}       | ${prefixFixture}
  ${'a.func d=x x        '}  | ${'=d @a ?b ?c'}       | ${prefixFixture}
  ${'a.func d=x x        '}  | ${'=d @a ?b ?c'}       | ${prefixFixture}
  ${'a.func d=x x x      '}  | ${'=d @a @b ?c'}       | ${prefixFixture}
  ${'a.func d=x x x x    '}  | ${'=d @a @b @c'}       | ${prefixFixture}
  ${'a.func x            '}  | ${'@a ?b ?c ?d'}       | ${prefixFixture}
  ${'a.func x b=x c=x    '}  | ${'@a =b =c ?d'}       | ${prefixFixture}
  ${'a.func x b=x x      '}  | ${'@a =b @c ?d'}       | ${prefixFixture}
  ${'a.func x d=x        '}  | ${'@a ?b ?c =d'}       | ${prefixFixture}
  ${'a.func x x          '}  | ${'@a @b ?c ?d'}       | ${prefixFixture}
  ${'a.func x x x        '}  | ${'@a @b @c ?d'}       | ${prefixFixture}
  ${'a.func x x x x      '}  | ${'@a @b @c @d'}       | ${prefixFixture}
  ${'a.func a=x x m=x    '}  | ${'=a @b ?c ?d =m'}    | ${prefixFixture}
  ${'x + y'}                 | ${'@lhs @rhs'}         | ${infixFixture}
  ${'x +'}                   | ${'@lhs ?rhs'}         | ${infixFixture}
`(
  "Creating argument application's info: $expression $expectedPattern",
  ({
    expression,
    expectedPattern,
    fixture: { allowInfix, mockSuggestion, argsParameters },
  }: TestData) => {
    const ast = Ast.parse(expression.trim())

    const configuration: widgetCfg.FunctionCall = {
      kind: 'FunctionCall',
      parameters: argsParameters,
    }

    const interpreted = interpretCall(ast, allowInfix)
    const call = ArgumentApplication.FromInterpretedWithInfo(interpreted, {
      suggestion: mockSuggestion,
      widgetCfg: configuration,
      subjectAsSelf: true,
    })
    assert(call instanceof ArgumentApplication)
    expect(printArgPattern(call)).toEqual(expectedPattern)
    checkArgsConfig(call, argsParameters)
  },
)

interface TestCase {
  description: string
  code: string
  /** Index of sub-application for which the MethodCallInfo is available. */
  subapplicationIndex: number
  /** Not applied arguments in available MethodCallInfo. */
  notAppliedArguments: number[]
  /** Not applied arguments expected for the whole expression. */
  expectedNotAppliedArguments: number[]
}

test.each([
  {
    description: 'Base case',
    code: 'Aggregate_Column.Sum',
    subapplicationIndex: 0,
    notAppliedArguments: [0, 1],
    expectedNotAppliedArguments: [0, 1],
  },
  {
    description: '1 arg, info on most inner subapplication.',
    code: 'Aggregate_Column.Sum x',
    subapplicationIndex: 1,
    notAppliedArguments: [0, 1],
    expectedNotAppliedArguments: [1],
  },
  {
    description: '2 args, info on most inner subapplication.',
    code: 'Aggregate_Column.Sum x y',
    subapplicationIndex: 2,
    notAppliedArguments: [0, 1],
    expectedNotAppliedArguments: [],
  },
  {
    description: '2 args, info on inner subapplication.',
    code: 'Aggregate_Column.Sum x y',
    subapplicationIndex: 1,
    notAppliedArguments: [1],
    expectedNotAppliedArguments: [],
  },
  {
    description: '2 args, notAppliedArguments are incorrectly empty.',
    code: 'Aggregate_Column.Sum x y',
    subapplicationIndex: 2,
    notAppliedArguments: [],
    expectedNotAppliedArguments: [],
  },
  {
    description: '1 arg, notAppliedArguments unsorted.',
    code: 'Aggregate_Column.Sum x',
    subapplicationIndex: 1,
    notAppliedArguments: [1, 0],
    expectedNotAppliedArguments: [1],
  },
  {
    description: '1 named arg.',
    code: 'Aggregate_Column.Sum as=x',
    subapplicationIndex: 1,
    notAppliedArguments: [0, 1],
    expectedNotAppliedArguments: [0],
  },
  {
    description: '2 named args.',
    code: 'Aggregate_Column.Sum as=x column=y',
    subapplicationIndex: 2,
    notAppliedArguments: [0, 1],
    expectedNotAppliedArguments: [],
  },
  {
    description: '1 wrongly named arg.',
    code: 'Aggregate_Column.Sum bla=x',
    subapplicationIndex: 1,
    notAppliedArguments: [0, 1],
    expectedNotAppliedArguments: [0, 1],
  },
  {
    description: '1 named & 1 unnamed args.',
    code: 'Aggregate_Column.Sum as=x y',
    subapplicationIndex: 2,
    notAppliedArguments: [0, 1],
    expectedNotAppliedArguments: [],
  },
] as TestCase[])(
  'Getting MethodCallInfo for sub-applications: $description',
  ({ code, subapplicationIndex, notAppliedArguments, expectedNotAppliedArguments }: TestCase) => {
    const { db, expectedMethodCall, expectedSuggestion, setExpressionInfo } =
      prepareMocksForGetMethodCallTest()
    const ast = Ast.parse(code)
    db.updateExternalIds(ast)
    const subApplication = nthSubapplication(ast, subapplicationIndex)
    assert(subApplication)
    setExpressionInfo(subApplication.id, {
      typename: undefined,
      methodCall: { ...expectedMethodCall, notAppliedArguments },
      payload: { type: 'Pending' } as ExpressionUpdatePayload,
      profilingInfo: [],
    })

    const info = getMethodCallInfoRecursively(ast, db)
    expect(info?.methodCall).toEqual({
      ...expectedMethodCall,
      notAppliedArguments: expectedNotAppliedArguments,
    })
    expect(info?.suggestion).toEqual(expectedSuggestion)
  },
)

interface ArgsTestCase extends TestCase {
  expectedSameIds: Array<[string, string]>
}

test.each([
  {
    description: 'Base case',
    code: 'Aggregate_Column.Sum',
    subapplicationIndex: 0,
    notAppliedArguments: [0, 1],
    expectedNotAppliedArguments: [0, 1],
    expectedSameIds: [['0', undefined]],
  },
  {
    description: '1 arg, info on most inner subapplication.',
    code: 'Aggregate_Column.Sum x',
    subapplicationIndex: 1,
    notAppliedArguments: [0, 1],
    expectedNotAppliedArguments: [1],
    expectedSameIds: [['0', 'column']],
  },
  {
    description: '2 args, info on most inner subapplication.',
    code: 'Aggregate_Column.Sum x y',
    subapplicationIndex: 2,
    notAppliedArguments: [0, 1],
    expectedNotAppliedArguments: [],
    expectedSameIds: [
      ['0', 'column'],
      ['1', 'as'],
    ],
  },
  {
    description: '2 args, info on inner subapplication.',
    code: 'Aggregate_Column.Sum x y',
    subapplicationIndex: 1,
    notAppliedArguments: [1],
    expectedNotAppliedArguments: [],
    expectedSameIds: [
      ['0', 'column'],
      ['1', 'as'],
    ],
  },
  {
    description: '2 args, notAppliedArguments are incorrectly empty.',
    code: 'Aggregate_Column.Sum x y',
    subapplicationIndex: 2,
    notAppliedArguments: [],
    expectedNotAppliedArguments: [],
    expectedSameIds: [
      ['0', 'column'],
      ['1', 'as'],
    ],
  },
  {
    description: '1 arg, notAppliedArguments unsorted.',
    code: 'Aggregate_Column.Sum x',
    subapplicationIndex: 1,
    notAppliedArguments: [1, 0],
    expectedNotAppliedArguments: [1],
    expectedSameIds: [
      ['0', 'column'],
      ['1', null],
    ],
  },
  {
    description: '1 named arg.',
    code: 'Aggregate_Column.Sum as=x',
    subapplicationIndex: 1,
    notAppliedArguments: [0, 1],
    expectedNotAppliedArguments: [0],
    expectedSameIds: [
      ['1', 'as'],
      ['column', null],
    ],
  },
  {
    description: '2 named args.',
    code: 'Aggregate_Column.Sum as=x column=y',
    subapplicationIndex: 2,
    notAppliedArguments: [0, 1],
    expectedNotAppliedArguments: [],
    expectedSameIds: [
      ['0', 'as'],
      ['1', 'column'],
    ],
  },
  {
    description: '1 wrongly named arg.',
    code: 'Aggregate_Column.Sum bla=x',
    subapplicationIndex: 1,
    notAppliedArguments: [0, 1],
    expectedNotAppliedArguments: [0, 1],
    expectedSameIds: [['2', 'bla']],
  },
  {
    description: '1 named & 1 unnamed args.',
    code: 'Aggregate_Column.Sum as=x y',
    subapplicationIndex: 2,
    notAppliedArguments: [0, 1],
    expectedNotAppliedArguments: [],
    expectedSameIds: [['0', 'as']],
  },
  {
    description: '1 unnamed & 1 named args.',
    code: 'Aggregate_Column.Sum y as=x',
    subapplicationIndex: 2,
    notAppliedArguments: [0, 1],
    expectedNotAppliedArguments: [],
    expectedSameIds: [
      ['1', 'as'],
      ['0', 'column'],
    ],
  },
] as ArgsTestCase[])(
  'Computing IDs of arguments: $description',
  ({ code, subapplicationIndex, notAppliedArguments, expectedSameIds }: ArgsTestCase) => {
    const { db, expectedMethodCall, setExpressionInfo } = prepareMocksForGetMethodCallTest()
    const ast = Ast.parse(code)
    const subApplication = nthSubapplication(ast, subapplicationIndex)
    assert(subApplication)
    db.updateExternalIds(ast)
    setExpressionInfo(subApplication.id, {
      typename: undefined,
      methodCall: { ...expectedMethodCall, notAppliedArguments },
      payload: { type: 'Pending' } as ExpressionUpdatePayload,
      profilingInfo: [],
    })

    const info = getMethodCallInfoRecursively(ast, db)
    const interpreted = interpretCall(ast)
    const res = ArgumentApplication.collectArgumentNamesAndUuids(interpreted, info)

    if (expectedSameIds) {
      for (const p of expectedSameIds) {
        if (p[1] === undefined) {
          const id = res[p[0]]
          assertEqual(undefined, id, `No ${[0]} ID found`)
        } else if (p[1] === null) {
          const id = res[p[0]]
          assertNotEqual(null, id, `One ${id} ID found`)
          for (const name in res) {
            if (name == p[0]) {
              continue
            }
            assertNotEqual(id, res[name], `No other ${id} found, testing ${name}`)
          }
        } else {
          const id1 = res[p[0]]
          const id2 = res[p[1]]
          assertEqual(id1, id2, `Checking ${p[0]} and ${p[1]}`)
        }
      }
    } else {
      fail('Undefined expectedSameIds')
    }
  },
)

function prepareMocksForGetMethodCallTest(): {
  db: GraphDb
  expectedMethodCall: MethodCall
  expectedSuggestion: SuggestionEntry
  setExpressionInfo: (id: AstId, info: ExpressionInfo) => void
} {
  const suggestionDb = new SuggestionDb()
  suggestionDb.set(1, makeModule('Standard.Table.Aggregate_Column'))
  suggestionDb.set(2, makeType('Standard.Table.Aggregate_Column.Aggregate_Column'))
  const con = makeConstructor('Standard.Table.Aggregate_Column.Aggregate_Column.Sum')
  con.arguments = [makeArgument('column', 'Any'), makeArgument('as', 'Any')]
  suggestionDb.set(3, con)
  const expectedSuggestion = suggestionDb.get(3)!
  const registry = ComputedValueRegistry.Mock()
  const db = GraphDb.Mock(registry, suggestionDb)
  const expectedMethodCall = {
    methodPointer: {
      module: 'Standard.Table.Aggregate_Column',
      definedOnType: 'Standard.Table.Aggregate_Column.Aggregate_Column',
      name: 'Sum',
    },
    notAppliedArguments: [0, 1],
  }
  const setExpressionInfo = (id: AstId, info: ExpressionInfo) =>
    registry.db.set(db.idToExternal(id)!, info)
  return { db, expectedMethodCall, expectedSuggestion, setExpressionInfo }
}

/** Nth sub-application of the Ast.App call chain. 0th is a `root` itself. */
function nthSubapplication(root: Ast.Ast, n: number): Ast.Ast | undefined {
  let current: Ast.Ast | undefined = root
  for (let i = 0; i < n; i++) {
    current = current instanceof Ast.App ? current.function : undefined
  }
  return current
}

function printArgPattern(application: ArgumentApplication | Ast.Ast) {
  const parts: string[] = []
  let current: ArgumentApplication['target'] = application

  while (current instanceof ArgumentApplication) {
    const sigil =
      current.argument instanceof ArgumentPlaceholder ? '?'
      : current.appTree instanceof Ast.App && current.appTree.argumentName ? '='
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
