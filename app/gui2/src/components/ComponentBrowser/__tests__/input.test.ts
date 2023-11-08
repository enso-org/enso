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
  // TODO[ao] test cases for #7926
  // [
  //   'operator1.',
  //   10,
  //   { type: 'insert', position: 10, oprApp: ['operator1', '.', null] },
  //   { selfType: 'Standard.Base.Number' },
  // ],
  // [
  //   'operator2.',
  //   10,
  //   { type: 'insert', position: 10, oprApp: ['operator2', '.', null] },
  //   // No self type, as the operator2 local is from another module
  //   { qualifiedNamePattern: 'operator2' },
  // ],
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
    expFiltering: { pattern?: string; qualifiedNamePattern?: string; selfType?: string },
  ) => {
    // TODO[ao] See above commented cases for #7926
    // const db = SuggestionDb.mock([
    //   makeLocal('local.Project', 'operator1', 'Standard.Base.Number'),
    //   makeLocal('local.Project.Another_Module', 'operator2', 'Standard.Base.Text'),
    //   makeType('local.Project.operator1'),
    //   makeLocal('local.Project', 'operator3', 'Standard.Base.Text'),
    // ])
    const input = useComponentBrowserInput()
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
    expect(filter.selfType).toStrictEqual(expFiltering.selfType)
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
    const input = useComponentBrowserInput()
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
