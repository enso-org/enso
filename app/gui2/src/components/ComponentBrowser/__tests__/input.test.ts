import {
  makeCon,
  makeLocal,
  makeMethod,
  makeModuleMethod,
  makeStaticMethod,
  type SuggestionEntry,
} from '@/stores/suggestionDatabase/entry'
import { readAstSpan } from '@/util/ast'
import { expect, test } from 'vitest'
import { Input } from '../input'

test.each([
  ['', 0, { type: 'insert', position: 0 }, {}],
  [
    'Data.',
    5,
    { type: 'insert', position: 5, accessorChain: ['Data', '.', null] },
    { qualifiedNamePattern: 'Data' },
  ],
  ['Data.', 4, { type: 'changeIdentifier', identifier: 'Data' }, { pattern: 'Data' }],
  [
    'Data.read',
    5,
    { type: 'insert', position: 5, accessorChain: ['Data', '.', 'read'] },
    { qualifiedNamePattern: 'Data' },
  ],
  [
    'Data.read',
    7,
    { type: 'changeIdentifier', identifier: 'read', accessorChain: ['Data', '.', 'read'] },
    { pattern: 're', qualifiedNamePattern: 'Data' },
  ],
])(
  "Input context and filtering, when content is '%s' and cursor at %i",
  (
    code,
    cursorPos,
    expContext: {
      type: string
      position?: number
      accessorChain?: (string | null)[]
      identifier?: string
      literal?: string
    },
    expFiltering: { pattern?: string; qualifiedNamePattern?: string },
  ) => {
    const input = new Input()
    input.code.value = code
    input.selection.value = { start: cursorPos, end: cursorPos }
    const context = input.context.value
    const filter = input.filter.value
    expect(context.type).toStrictEqual(expContext.type)
    switch (context.type) {
      case 'insert':
        expect(context.position).toStrictEqual(expContext.position)
        expect(
          context.accessOpr != null
            ? Array.from(context.accessOpr.componentsReprs(code))
            : undefined,
        ).toStrictEqual(expContext.accessorChain)
        break
      case 'changeIdentifier':
        expect(readAstSpan(context.identifier, code)).toStrictEqual(expContext.identifier)
        expect(
          context.accessOpr != null
            ? Array.from(context.accessOpr.componentsReprs(code))
            : undefined,
        ).toStrictEqual(expContext.accessorChain)
        break
      case 'changeLiteral':
        expect(readAstSpan(context.literal, code)).toStrictEqual(expContext.literal)
    }
    expect(filter.pattern).toStrictEqual(expFiltering.pattern)
    expect(filter.qualifiedNamePattern).toStrictEqual(expFiltering.qualifiedNamePattern)
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
  // {
  //   code: 'Dat . V . ',
  //   suggestion: makeStaticMethod('Standard.Base.Data.Vector.new'),
  //   expected: 'Data . Vector . new ',
  // },
  // {
  //   code: 'Dat . V .',
  //   suggestion: makeStaticMethod('Standard.Base.Data.Vector.new'),
  //   expected: 'Data . Vector . new ',
  // },
  {
    code: '(type_method some_arg).Vector.',
    suggestion: makeStaticMethod('Standard.Base.Data.Vector.new'),
    expected: '(type_method some_arg).Vector.new ',
  },
]

function makeComplexCase(prefix: string, suffix: string): ApplySuggestionCase[] {
  return Array.from(baseCases, (aCase) => {
    return {
      ...aCase,
      code: `${prefix}${aCase.code}${suffix}`,
      // We must remove trailing space. The suffix should contain one.
      expected: `${prefix}${aCase.expected.substring(0, aCase.expected.length - 1)}${suffix}`,
      cursorPos: prefix.length + aCase.code.length,
      expectedCursorPos: prefix.length + aCase.expected.length,
    }
  })
}

const insideInfixCases = makeComplexCase('333 + ', ' + 444')
const insideBracketsCases = makeComplexCase('(', ')')
const insideListCases = makeComplexCase('[foo, ', ', bar]')

test.each(baseCases.concat(insideInfixCases).concat(insideBracketsCases).concat(insideListCases))(
  'Applying suggestion $suggestion.name to $code',
  ({ code, cursorPos, suggestion, expected, expectedCursorPos }) => {
    cursorPos = cursorPos ?? code.length
    expectedCursorPos = expectedCursorPos ?? expected.length
    const input = new Input()
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
