import { GraphDb, mockNode } from '@/stores/graph/graphDatabase'
import {
  makeCon,
  makeLocal,
  makeMethod,
  makeModule,
  makeModuleMethod,
  makeStaticMethod,
  type SuggestionEntry,
} from '@/stores/suggestionDatabase/entry'
import { readAstSpan } from '@/util/ast'
import { ComputedValueRegistry } from '@/util/computedValueRegistry'
import type { ExprId } from 'shared/yjsModel'
import { expect, test } from 'vitest'
import { useComponentBrowserInput } from '../input'

test.each([
  ['', 0, { type: 'insert', position: 0 }, {}],
  [
    'Data.',
    5,
    { type: 'insert', position: 5, oprApp: ['Data', '.', null] },
    { qualifiedNamePattern: 'Data' },
  ],
  ['Data.', 4, { type: 'changeIdentifier', identifier: 'Data' }, { pattern: 'Data' }],
  [
    'Data.read',
    5,
    { type: 'insert', position: 5, oprApp: ['Data', '.', 'read'] },
    { qualifiedNamePattern: 'Data' },
  ],
  [
    'Data.read',
    7,
    { type: 'changeIdentifier', identifier: 'read', oprApp: ['Data', '.', 'read'] },
    { pattern: 're', qualifiedNamePattern: 'Data' },
  ],
  [
    'Base . Data .',
    13,
    { type: 'insert', position: 13, oprApp: ['Base . Data', '.', null] },
    { qualifiedNamePattern: 'Base.Data' },
  ],
  ['2 +', 3, { type: 'insert', position: 3, oprApp: ['2', '+', null] }, {}],
  ['2 + 3', 5, { type: 'changeLiteral', literal: '3' }, { pattern: '3' }],
  [
    'operator1.',
    10,
    { type: 'insert', position: 10, oprApp: ['operator1', '.', null] },
    { selfArg: { type: 'known', typename: 'Standard.Base.Number' } },
  ],
  [
    'operator2.',
    10,
    { type: 'insert', position: 10, oprApp: ['operator2', '.', null] },
    { selfArg: { type: 'unknown' } },
  ],
  [
    'operator3.',
    10,
    { type: 'insert', position: 10, oprApp: ['operator3', '.', null] },
    // No self type, as there is no operator3 node in current graph
    { qualifiedNamePattern: 'operator3' },
  ],
  [
    'operator1 -> operator1.',
    23,
    { type: 'insert', position: 23, oprApp: ['operator1', '.', null] },
    { selfArg: { type: 'unknown' } },
  ],
  [
    'operator2 -> operator1.',
    23,
    { type: 'insert', position: 23, oprApp: ['operator1', '.', null] },
    { selfArg: { type: 'known', typename: 'Standard.Base.Number' } },
  ],
])(
  "Input context and filtering, when content is '%s' and cursor at %i",
  (
    code,
    cursorPos,
    expContext: {
      type: string
      position?: number
      oprApp?: (string | null)[]
      identifier?: string
      literal?: string
    },
    expFiltering: {
      pattern?: string
      qualifiedNamePattern?: string
      selfArg?: { type: string; typename?: string }
    },
  ) => {
    const operator1Id: ExprId = '3d0e9b96-3ca0-4c35-a820-7d3a1649de55' as ExprId
    const operator2Id: ExprId = '5eb16101-dd2b-4034-a6e2-476e8bfa1f2b' as ExprId
    const computedValueRegistryMock = ComputedValueRegistry.Mock()
    computedValueRegistryMock.db.set(operator1Id, {
      typename: 'Standard.Base.Number',
      methodCall: undefined,
      payload: { type: 'Value' },
      profilingInfo: [],
    })
    const mockGraphDb = GraphDb.Mock(computedValueRegistryMock)
    mockGraphDb.nodeIdToNode.set(operator1Id, mockNode('operator1', operator1Id))
    mockGraphDb.nodeIdToNode.set(operator2Id, mockNode('operator2', operator2Id))

    const input = useComponentBrowserInput(mockGraphDb)
    input.code.value = code
    input.selection.value = { start: cursorPos, end: cursorPos }
    const context = input.context.value
    const filter = input.filter.value
    expect(context.type).toStrictEqual(expContext.type)
    switch (context.type) {
      case 'insert':
        expect(context.position).toStrictEqual(expContext.position)
        expect(
          context.oprApp != null ? Array.from(context.oprApp.componentsReprs(code)) : undefined,
        ).toStrictEqual(expContext.oprApp)
        break
      case 'changeIdentifier':
        expect(readAstSpan(context.identifier, code)).toStrictEqual(expContext.identifier)
        expect(
          context.oprApp != null ? Array.from(context.oprApp.componentsReprs(code)) : undefined,
        ).toStrictEqual(expContext.oprApp)
        break
      case 'changeLiteral':
        expect(readAstSpan(context.literal, code)).toStrictEqual(expContext.literal)
    }
    expect(filter.pattern).toStrictEqual(expFiltering.pattern)
    expect(filter.qualifiedNamePattern).toStrictEqual(expFiltering.qualifiedNamePattern)
    expect(filter.selfArg).toStrictEqual(expFiltering.selfArg)
  },
)

interface ApplySuggestionCase {
  code: string
  cursorPos?: number
  suggestion: SuggestionEntry
  expected: string
  expectedCursorPos?: number
}

const baseCases: ApplySuggestionCase[] = [
  {
    code: '',
    suggestion: makeLocal('local.Project.Main', 'operator1'),
    expected: 'operator1 ',
  },
  {
    code: '',
    suggestion: makeMethod('Standard.Base.Data.Vector.get'),
    expected: '_.get ',
  },
  {
    code: '',
    suggestion: makeStaticMethod('Standard.Base.Data.Vector.new'),
    expected: 'Vector.new ',
  },
  {
    code: '',
    suggestion: makeModuleMethod('Standard.Base.Data.read'),
    expected: 'Data.read ',
  },
  {
    code: '',
    suggestion: makeCon('local.Project.Main.Option.Some'),
    expected: 'Option.Some ',
  },
  {
    code: 'operator1.',
    suggestion: makeMethod('Standard.Base.Data.Vector.get'),
    expected: 'operator1.get ',
  },
  {
    code: 'Data.Vector.',
    suggestion: makeStaticMethod('Standard.Base.Data.Vector.new'),
    expected: 'Data.Vector.new ',
  },
  {
    code: 'Dat.V.',
    suggestion: makeStaticMethod('Standard.Base.Data.Vector.new'),
    expected: 'Data.Vector.new ',
  },
  {
    code: 'Dat . V .',
    suggestion: makeStaticMethod('Standard.Base.Data.Vector.new'),
    expected: 'Data . Vector . new ',
  },
  {
    code: '(type_method some_arg).Vector.',
    suggestion: makeStaticMethod('Standard.Base.Data.Vector.new'),
    expected: '(type_method some_arg).Vector.new ',
  },
  {
    code: '',
    suggestion: makeModule('local.Project.Module'),
    expected: 'local.Project.Module.',
  },
  {
    code: 'local.Project.',
    suggestion: makeModule('local.Project.Module'),
    expected: 'local.Project.Module.',
  },
  {
    code: 'Project.Mod',
    suggestion: makeModule('local.Project.Module'),
    expected: 'Project.Module.',
  },
  {
    code: 'a -> a.',
    suggestion: makeMethod('Standard.Base.Data.Vector.get'),
    expected: 'a -> a.get ',
  },
]

function makeComplexCase(prefix: string, suffix: string): ApplySuggestionCase[] {
  return Array.from(baseCases, (aCase) => {
    return {
      ...aCase,
      code: `${prefix}${aCase.code}${suffix}`,
      // We must remove trailing space. The suffix should contain one.
      expected: `${prefix}${aCase.expected.trimEnd()}${suffix}`,
      cursorPos: prefix.length + aCase.code.length,
      expectedCursorPos: prefix.length + aCase.expected.length,
    }
  })
}

const insideInfixCases = makeComplexCase('333 + ', ' + 444')
const insideBracketsCases = makeComplexCase('(', ')')
const insideListCases = makeComplexCase('[foo, ', ', bar]')

test.each([
  ...baseCases,
  ...insideInfixCases,
  ...insideBracketsCases,
  ...insideListCases,
  // space is added to operands if needed.
  {
    code: '2+',
    suggestion: makeLocal('local.Project.Main', 'operator2'),
    expected: '2+operator2 ',
  },
  {
    code: '2 +',
    suggestion: makeLocal('local.Project.Main', 'operator2'),
    expected: '2 + operator2 ',
  },
])(
  'Applying suggestion $suggestion.name to $code',
  ({ code, cursorPos, suggestion, expected, expectedCursorPos }) => {
    cursorPos = cursorPos ?? code.length
    expectedCursorPos = expectedCursorPos ?? expected.length

    const input = useComponentBrowserInput(GraphDb.Mock())
    input.code.value = code
    input.selection.value = { start: cursorPos, end: cursorPos }
    input.applySuggestion(suggestion)
    expect(input.code.value).toEqual(expected)
    expect(input.selection.value).toStrictEqual({
      start: expectedCursorPos,
      end: expectedCursorPos,
    })
  },
)
