import { TypeInfo } from '$/providers/openedProjects/project/computedValueRegistry'
import { SuggestionDb } from '$/providers/openedProjects/suggestionDatabase'
import { type SuggestionEntry } from '$/providers/openedProjects/suggestionDatabase/entry'
import {
  makeConstructor,
  makeFunction,
  makeLocal,
  makeMethod,
  makeModule,
  makeModuleMethod,
  makeStaticMethod,
} from '$/providers/openedProjects/suggestionDatabase/mockSuggestion'
import { Filtering, type MatchResult } from '@/components/ComponentBrowser/filtering'
import { stdPath } from '@/util/projectPath'
import { qnLastSegment } from '@/util/qualifiedName'
import type { Opt } from 'enso-common/src/utilities/data/opt'
import { describe, expect, test } from 'vitest'

test.each([
  makeModuleMethod('Standard.Base.Data.read', { group: 'Standard.Base.MockGroup1' }),
  makeModuleMethod('Standard.Base.Data.write', { group: 'Standard.Base.MockGroup1' }),
  makeStaticMethod('Standard.Base.Data.Vector.Vector.new', { group: 'Standard.Base.MockGroup2' }),
  makeModuleMethod('Standard.Base.Data.read_text'),
  makeStaticMethod('local.Project.Foo.new'),
  makeStaticMethod('local.Project.Internalization.internalize'),
])('$name entry is in the CB main view', (entry) => {
  const filtering = new Filtering({})
  const db = new SuggestionDb()
  expect(filtering.filter(entry, db)).not.toBeNull()
})

test.each([
  makeModuleMethod('Standard.Base.Data.Vector.some_method'), // not in top group
  makeMethod('Standard.Base.Data.Vector.Vector.get', { group: 'Standard.Base.MockGroup2' }), // not static method
  makeModule('Standard.Base.Data.Vector'), // Not top module
  makeModule('local.New_Project'), // Main module
  makeModule('Standard.Base.Data'), // Top module
  makeStaticMethod('Standard.Base.Internal.Foo.bar'), // Internal method
])('$name entry is not in the CB main view', (entry) => {
  const filtering = new Filtering({})
  const db = new SuggestionDb()
  expect(filtering.filter(entry, db)).toBeNull()
})

test.each`
  visibleTypes                                                        | entry1Matched | entry2Matched
  ${['Standard.Base.Data.Table']}                                     | ${false}      | ${true}
  ${['Standard.Base.Data.Vector.Vector']}                             | ${true}       | ${false}
  ${['Standard.Base.Data.Vector.Vector', 'Standard.Base.Data.Table']} | ${true}       | ${true}
  ${['Standard.Base.Data.Table', 'Standard.Base.Data.Vector.Vector']} | ${true}       | ${true}
`(
  `Visible types are taken into account when filtering: $visibleTypes`,
  ({ visibleTypes, entry1Matched, entry2Matched }) => {
    const entry1 = makeMethod('Standard.Base.Data.Vector.Vector.get')
    const entry2 = makeMethod('Standard.Base.Data.Table.get')
    const filtering = new Filtering({
      selfArg: {
        type: 'known',
        typeInfo: TypeInfo.fromParsedTypes(visibleTypes.map(stdPath), [])!,
        ancestors: [],
      },
    })
    const db = new SuggestionDb()
    expect(filtering.filter(entry1, db)).toEqual(entry1Matched ? { score: 0 } : null)
    expect(filtering.filter(entry2, db)).toEqual(entry2Matched ? { score: 0 } : null)
  },
)

test('Filtering methods with no self type information', () => {
  const entry1 = makeMethod('Standard.Base.Data.Vector.Vector.get')
  const entry2 = makeMethod('Standard.Base.Data.Table.get')
  const filteringWithAnySelfType = new Filtering({
    selfArg: { type: 'unknown' },
  })
  const db = new SuggestionDb()
  expect(filteringWithAnySelfType.filter(entry1, db)).not.toBeNull()
  expect(filteringWithAnySelfType.filter(entry2, db)).not.toBeNull()
  const filteringWithoutSelfType = new Filtering({ pattern: 'get' })
  expect(filteringWithoutSelfType.filter(entry1, db)).toBeNull()
  expect(filteringWithoutSelfType.filter(entry2, db)).toBeNull()
})

test('`Any` type methods taken into account when filtering', () => {
  const entry1 = makeMethod('Standard.Base.Data.Vector.Vector.get')
  const entry2 = makeMethod('Standard.Base.Any.Any.to_string')
  const filtering = new Filtering({
    selfArg: {
      type: 'known',
      typeInfo: TypeInfo.fromParsedTypes([stdPath('Standard.Base.Data.Vector.Vector')], [])!,
      ancestors: [],
    },
  })
  const db = new SuggestionDb()
  expect(filtering.filter(entry1, db)).not.toBeNull()
  expect(filtering.filter(entry2, db)).not.toBeNull()

  const filteringWithoutSelfType = new Filtering({})
  expect(filteringWithoutSelfType.filter(entry1, db)).toBeNull()
  expect(filteringWithoutSelfType.filter(entry2, db)).toBeNull()
})

test('Hidden self types and ancestors are taken into account when filtering', () => {
  const entry1 = makeMethod('Standard.Base.Data.Numbers.Float.abs')
  const entry2 = makeMethod('Standard.Base.Data.Numbers.Number.sqrt')
  const hiddenSelfType = 'Standard.Base.Data.Numbers.Number'
  const filteringWithHiddenSelfType = new Filtering({
    selfArg: {
      type: 'known',
      typeInfo: TypeInfo.fromParsedTypes(
        [stdPath('Standard.Base.Data.Numbers.Float')],
        [stdPath(hiddenSelfType)],
      )!,
      ancestors: [],
    },
  })
  const db = new SuggestionDb()
  expect(filteringWithHiddenSelfType.filter(entry1, db)).toEqual({ score: 0 })
  expect(filteringWithHiddenSelfType.filter(entry2, db)).toEqual({
    score: 1,
    fromType: stdPath(hiddenSelfType),
  })

  const filteringWithoutSelfType = new Filtering({})
  expect(filteringWithoutSelfType.filter(entry1, db)).toBeNull()
  expect(filteringWithoutSelfType.filter(entry2, db)).toBeNull()

  const filteringWithAncestors = new Filtering({
    selfArg: {
      type: 'known',
      typeInfo: TypeInfo.fromParsedTypes([stdPath('Standard.Base.Data.Numbers.Float')], [])!,
      ancestors: [stdPath(hiddenSelfType)],
    },
  })
  expect(filteringWithAncestors.filter(entry1, db)).toEqual({ score: 0 })
  expect(filteringWithAncestors.filter(entry2, db)).toEqual({
    score: 1,
    fromType: undefined,
  })
})

test.each([
  makeModule('Standard.Base.Data.Vector'),
  makeStaticMethod('Standard.Base.Data.Vector.Vector.new'),
  makeConstructor('Standard.Base.Data.Vector.Vector.Vector_Con'),
  makeLocal('Standard.Base.Data.Vector', 'get'),
  makeFunction('Standard.Base.Data.Vector', 'func'),
  makeMethod('Standard.Base.Data.Vector.Vecto.get'),
  makeMethod('Standard.Base.Data.Vector.Vector2.get'),
])('$name is filtered out when Vector self type is specified', (entry) => {
  const filtering = new Filtering({
    selfArg: {
      type: 'known',
      typeInfo: TypeInfo.fromParsedTypes([stdPath('Standard.Base.Data.Vector.Vector')], [])!,
      ancestors: [],
    },
  })
  const db = new SuggestionDb()
  expect(filtering.filter(entry, db)).toBeNull()
})

test.each`
  name            | pattern
  ${'bar'}        | ${'foo'}
  ${'barfoo'}     | ${'foo'}
  ${'fo'}         | ${'foo'}
  ${'bar_fo_bar'} | ${'foo'}
  ${'bar'}        | ${'+'}
  ${'*'}          | ${'+'}
  ${'<='}         | ${'='}
`('$name is not matched by pattern $pattern', ({ name, pattern }) => {
  const entry = makeModuleMethod(`local.Project.${name}`)
  const filtering = new Filtering({ pattern })
  const db = new SuggestionDb()
  expect(filtering.filter(entry, db)).toBeNull()
})

function matchedText(ownerName: string, name: string, matchResult: MatchResult) {
  name = matchResult.matchedAlias ?? name
  const ownerPart = matchResult.ownerNameRanges?.map((range) => range.slice(ownerName)).join('')
  const namePart = matchResult.nameRanges?.map((range) => range.slice(name)).join('')
  return ownerPart ? `${ownerPart}.${namePart}` : `${namePart}`
}

type MatchingTestCase = {
  pattern: string
  matchedSorted: { module?: string; name: string; aliases?: string[] }[]
  notMatched: { module?: string; name: string; aliases?: string[] }[]
}

// In this test, `matchedSorted` are specified in expected score ascending order.
test.each<MatchingTestCase>([
  {
    pattern: 'foo',
    matchedSorted: [
      { name: 'foo' }, // exact match
      { name: 'foobar' }, // name start match
      { name: 'bar', aliases: ['baz', 'foo'] }, // exact alias match
      { name: 'bar', aliases: ['bazbar', 'foobar'] }, // alias start match
      { name: 'bar_foo' }, // name word exact match
      { name: 'baz_foobar' }, // name word start match
      { name: 'bar', aliases: ['bar_foo'] }, // alias word exact match
      { name: 'bar', aliases: ['baz_foobar'] }, // alias word start match
      { name: 'frequent_objective_objections' }, // initials match
      { name: 'bar', aliases: ['frequent_objective_objections'] }, // alias initials match
    ],
    notMatched: [
      { name: 'bar' },
      { name: 'fo' },
      { name: 'fo_o' },
      { name: 'bar', aliases: ['baz'] },
      { name: 'bar', aliases: ['fo', 'fo_o'] },
    ],
  },
  {
    pattern: 'foo_bar',
    matchedSorted: [
      { name: 'foo_bar' }, // exact match
      { name: 'foo_xyz_barabc' }, // first word exact match
      { name: 'fooabc_barabc' }, // first word match
      { name: 'bar', aliases: ['foo_bar', 'foo'] }, // exact alias match
      { name: 'bar', aliases: ['foo', 'foo_xyz_barabc'] }, // alias first word exact match
      { name: 'bar', aliases: ['foo', 'fooabc_barabc'] }, // alias first word match
      { name: 'xyz_foo_abc_bar_xyz' }, // exact word match
      { name: 'xyz_fooabc_abc_barabc_xyz' }, // non-exact word match
      { name: 'bar', aliases: ['xyz_foo_abc_bar_xyz'] }, // alias word exact match
      { name: 'bar', aliases: ['xyz_fooabc_abc_barabc_xyz'] }, // alias word start match
    ],
    notMatched: [
      { name: 'foo' },
      { name: 'bar' },
      { name: 'fo_bar' },
      { name: 'foo_ba', aliases: ['baz'] },
    ],
  },
  {
    pattern: 'foo bar',
    matchedSorted: [
      { name: 'foo_bar' }, // exact match
      { name: 'foo_xyz_barabc' }, // first word exact match
      { name: 'fooabc_barabc' }, // first word match
      { name: 'bar', aliases: ['foo bar', 'foo'] }, // exact alias match
      { name: 'bar', aliases: ['foo', 'foo_xyz_barabc'] }, // alias first word exact match
      { name: 'bar', aliases: ['foo', 'fooabc barabc'] }, // alias first word match
      { name: 'xyz_foo_abc_bar_xyz' }, // exact word match
      { name: 'xyz_fooabc_abc_barabc_xyz' }, // non-exact word match
      { name: 'bar', aliases: ['xyz_foo_abc_bar_xyz'] }, // alias word exact match
      { name: 'bar', aliases: ['xyz_fooabc_abc_barabc_xyz'] }, // alias word start match
    ],
    notMatched: [
      { name: 'foo' },
      { name: 'bar' },
      { name: 'fo_bar' },
      { name: 'foo_ba', aliases: ['baz'] },
    ],
  },
  {
    pattern: 'ma.foo',
    matchedSorted: [
      { module: 'local.Project.Ma', name: 'foo' }, // exact match
      { module: 'local.Project.Main', name: 'foo' }, // name exact match and owner name start match
      { module: 'local.Project.Ma', name: 'foobar' }, // module exact match and name start match
      { module: 'local.Project.Ma', name: 'bar', aliases: ['baz', 'foo'] }, // exact alias match
      { module: 'local.Project.Main', name: 'bar', aliases: ['baz', 'foo'] }, // exact alias match, but nonexact owner match
      { module: 'local.Project.Main', name: 'bar', aliases: ['bazbar', 'foobar'] }, // alias start match
      { name: 'bar_foo' }, // name word exact match
      { name: 'baz_foobar' }, // name word start match
      { name: 'bar', aliases: ['bar_foo'] }, // alias word exact match
      { name: 'bar', aliases: ['baz_foobar'] }, // alias word start match
      { name: 'frequent_objective_objections' }, // initials match
      { name: 'bar', aliases: ['frequent_objective_objections'] }, // alias initials match
    ],
    notMatched: [
      { module: 'local.Project.Data', name: 'foo' },
      { module: 'local.Pr', name: 'bar' },
    ],
  },
])('Matching pattern $pattern', ({ pattern, matchedSorted, notMatched }) => {
  const filtering = new Filtering({ pattern })
  const matchedSortedEntries = matchedSorted.map(({ name, aliases, module }) =>
    makeModuleMethod(`${module ?? 'local.Project'}.${name}`, { aliases: aliases ?? [] }),
  )
  const db = new SuggestionDb()
  const matchResults = matchedSortedEntries.map((entry) => filtering.filter(entry, db))
  // Checking matching entries
  function checkResult(entry: SuggestionEntry, result: Opt<MatchResult>) {
    expect(result, `Matching entry ${JSON.stringify(entry.definitionPath)}`).not.toBeNull()
    expect(
      matchedText(
        'memberOf' in entry ?
          entry.memberOf.path ?
            qnLastSegment(entry.memberOf.path)
          : 'Main'
        : '',
        entry.name,
        result!,
      )
        .toLowerCase()
        .replace(/ /g, '_'),
      `Matched text of entry ${entry.definitionPath}`,
    ).toEqual(pattern.toLowerCase().replace(/ /g, '_'))
  }
  expect(matchedSortedEntries.length).toBeGreaterThan(0)
  expect(matchedSortedEntries[0]).not.toBeNull()
  checkResult(matchedSortedEntries[0]!, matchResults[0])
  for (let i = 1; i < matchResults.length; i++) {
    checkResult(matchedSortedEntries[i]!, matchResults[i])
    expect(
      matchResults[i]!.score,
      `score('${matchedSortedEntries[i]!.definitionPath}') > score('${matchedSortedEntries[i - 1]!.definitionPath}')`,
    ).toBeGreaterThan(matchResults[i - 1]!.score)
  }

  // Checking non-matching entries
  for (const { module, name, aliases } of notMatched) {
    const entry = makeModuleMethod(`${module ?? 'local.Project'}.${name}`, {
      aliases: aliases ?? [],
    })
    const db = new SuggestionDb()
    expect(filtering.filter(entry, db), JSON.stringify(entry.definitionPath)).toBeNull()
  }
})

describe('Constructor fields accessors', () => {
  function createConstructor(name: string, isPrivate: boolean) {
    const res = makeConstructor(name, {
      args: [
        {
          name: 'object_node',
          reprType: 'Standard.Base.Any.Any',
          isSuspended: false,
          hasDefault: false,
        },
      ],
      ...(isPrivate ? { documentation: '---\nprivate: true\n---\n\n' } : {}),
    })
    expect(res.isPrivate).toBe(isPrivate)
    return res
  }

  const filtering = new Filtering({
    selfArg: {
      type: 'known',
      typeInfo: TypeInfo.fromParsedTypes([stdPath('Standard.Base.Data.Json.JS_Object')], [])!,
      ancestors: [],
    },
  })

  test('Public constructor fields are not filtered out', () => {
    const constructor = createConstructor('Standard.Base.Data.Json.JS_Object.Value', false)
    const fieldAccessor = makeMethod('Standard.Base.Data.Json.JS_Object.object_node')
    const typeMethod = makeMethod('Standard.Base.Data.Json.JS_Object.some_method')
    const db = new SuggestionDb()
    db.set(0, constructor)
    db.set(1, fieldAccessor)
    db.set(2, typeMethod)
    expect(filtering.filter(fieldAccessor, db)).not.toBeNull()
    expect(filtering.filter(typeMethod, db)).not.toBeNull()
  })

  test('Private constructor fields are filtered out', () => {
    const constructor = createConstructor('Standard.Base.Data.Json.JS_Object.Value', true)
    const fieldAccessor = makeMethod('Standard.Base.Data.Json.JS_Object.object_node')
    const typeMethod = makeMethod('Standard.Base.Data.Json.JS_Object.some_method')
    const db = new SuggestionDb()
    db.set(0, constructor)
    db.set(1, fieldAccessor)
    db.set(2, typeMethod)
    expect(filtering.filter(fieldAccessor, db)).toBeNull()
    expect(filtering.filter(typeMethod, db)).not.toBeNull()
  })

  test('At least one public constructor is enough to not filter out the accessor', () => {
    const constructor1 = createConstructor('Standard.Base.Data.Json.JS_Object.Value', true)
    const constructor2 = createConstructor('Standard.Base.Data.Json.JS_Object.PublicValue', false)
    const fieldAccessor = makeMethod('Standard.Base.Data.Json.JS_Object.object_node')
    const typeMethod = makeMethod('Standard.Base.Data.Json.JS_Object.some_method')
    const db = new SuggestionDb()
    db.set(0, constructor1)
    db.set(1, fieldAccessor)
    db.set(2, typeMethod)
    expect(filtering.filter(fieldAccessor, db)).toBeNull()
    expect(filtering.filter(typeMethod, db)).not.toBeNull()
    db.set(3, constructor2)
    expect(filtering.filter(fieldAccessor, db)).not.toBeNull()
    expect(filtering.filter(typeMethod, db)).not.toBeNull()
  })
})
