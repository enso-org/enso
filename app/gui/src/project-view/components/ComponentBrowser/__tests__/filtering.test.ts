import { Filtering, type MatchResult } from '@/components/ComponentBrowser/filtering'
import { SuggestionEntry } from '@/stores/suggestionDatabase/entry'
import {
  makeConstructor,
  makeFunction,
  makeLocal,
  makeMethod,
  makeModule,
  makeModuleMethod,
  makeStaticMethod,
} from '@/stores/suggestionDatabase/mockSuggestion'
import { qnLastSegment, QualifiedName } from '@/util/qualifiedName'
import { expect, test } from 'vitest'
import { Opt } from 'ydoc-shared/util/data/opt'

test.each([
  makeModuleMethod('Standard.Base.Data.read', { group: 'Standard.Base.MockGroup1' }),
  makeModuleMethod('Standard.Base.Data.write', { group: 'Standard.Base.MockGroup1' }),
  makeStaticMethod('Standard.Base.Data.Vector.Vector.new', { group: 'Standard.Base.MockGroup2' }),
  makeModuleMethod('Standard.Base.Data.read_text'),
  makeStaticMethod('local.Project.Foo.new'),
  makeStaticMethod('local.Project.Internalization.internalize'),
])('$name entry is in the CB main view', (entry) => {
  const filtering = new Filtering({})
  expect(filtering.filter(entry, [])).not.toBeNull()
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
  expect(filtering.filter(entry, [])).toBeNull()
})

test('An Instance method is shown when self arg matches', () => {
  const entry1 = makeMethod('Standard.Base.Data.Vector.Vector.get')
  const entry2 = makeMethod('Standard.Base.Data.Table.get')
  const filteringWithSelfType = new Filtering({
    selfArg: { type: 'known', typename: 'Standard.Base.Data.Vector.Vector' },
  })
  expect(filteringWithSelfType.filter(entry1, [])).not.toBeNull()
  expect(filteringWithSelfType.filter(entry2, [])).toBeNull()
  const filteringWithAnySelfType = new Filtering({
    selfArg: { type: 'unknown' },
  })
  expect(filteringWithAnySelfType.filter(entry1, [])).not.toBeNull()
  expect(filteringWithAnySelfType.filter(entry2, [])).not.toBeNull()
  const filteringWithoutSelfType = new Filtering({ pattern: 'get' })
  expect(filteringWithoutSelfType.filter(entry1, [])).toBeNull()
  expect(filteringWithoutSelfType.filter(entry2, [])).toBeNull()
})

test('`Any` type methods taken into account when filtering', () => {
  const entry1 = makeMethod('Standard.Base.Data.Vector.Vector.get')
  const entry2 = makeMethod('Standard.Base.Any.Any.to_string')
  const filtering = new Filtering({
    selfArg: { type: 'known', typename: 'Standard.Base.Data.Vector.Vector' },
  })
  expect(filtering.filter(entry1, [])).not.toBeNull()
  expect(filtering.filter(entry2, [])).not.toBeNull()

  const filteringWithoutSelfType = new Filtering({})
  expect(filteringWithoutSelfType.filter(entry1, [])).toBeNull()
  expect(filteringWithoutSelfType.filter(entry2, [])).toBeNull()
})

test('Additional self types are taken into account when filtering', () => {
  const entry1 = makeMethod('Standard.Base.Data.Numbers.Float.abs')
  const entry2 = makeMethod('Standard.Base.Data.Numbers.Number.sqrt')
  const additionalSelfType = 'Standard.Base.Data.Numbers.Number' as QualifiedName
  const filtering = new Filtering({
    selfArg: { type: 'known', typename: 'Standard.Base.Data.Numbers.Float' },
  })
  expect(filtering.filter(entry1, [additionalSelfType])).not.toBeNull()
  expect(filtering.filter(entry2, [additionalSelfType])).not.toBeNull()
  expect(filtering.filter(entry2, [])).toBeNull()

  const filteringWithoutSelfType = new Filtering({})
  expect(filteringWithoutSelfType.filter(entry1, [additionalSelfType])).toBeNull()
  expect(filteringWithoutSelfType.filter(entry2, [additionalSelfType])).toBeNull()
  expect(filteringWithoutSelfType.filter(entry1, [])).toBeNull()
  expect(filteringWithoutSelfType.filter(entry2, [])).toBeNull()
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
    selfArg: { type: 'known', typename: 'Standard.Base.Data.Vector.Vector' },
  })
  expect(filtering.filter(entry, [])).toBeNull()
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
  expect(filtering.filter(entry, [])).toBeNull()
})

function matchedText(ownerName: string, name: string, matchResult: MatchResult) {
  name = matchResult.matchedAlias ?? name
  const ownerPart = matchResult.ownerNameRanges?.reduce(
    (acc, range) => acc + ownerName.slice(range.start, range.end),
    '',
  )
  const namePart = matchResult.nameRanges?.reduce(
    (acc, range) => acc + name.slice(range.start, range.end),
    '',
  )
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
    pattern: 'pr.foo',
    matchedSorted: [
      { module: 'local.Pr', name: 'foo' }, // exact match
      { module: 'local.Project', name: 'foo' }, // name exact match and owner name start match
      { module: 'local.Pr', name: 'foobar' }, // module exact match and name start match
      { module: 'local.Pr', name: 'bar', aliases: ['baz', 'foo'] }, // exact alias match
      { module: 'local.Project', name: 'bar', aliases: ['baz', 'foo'] }, // exact alias match, but nonexact owner match
      { module: 'local.Project', name: 'bar', aliases: ['bazbar', 'foobar'] }, // alias start match
      { name: 'bar_foo' }, // name word exact match
      { name: 'baz_foobar' }, // name word start match
      { name: 'bar', aliases: ['bar_foo'] }, // alias word exact match
      { name: 'bar', aliases: ['baz_foobar'] }, // alias word start match
      { name: 'frequent_objective_objections' }, // initials match
      { name: 'bar', aliases: ['frequent_objective_objections'] }, // alias initials match
    ],
    notMatched: [
      { module: 'local.Project.Data', name: 'foo' },
      { module: 'local.Ploject', name: 'foo' },
      { module: 'local.Pr', name: 'bar' },
    ],
  },
])('Matching pattern $pattern', ({ pattern, matchedSorted, notMatched }) => {
  const filtering = new Filtering({ pattern })
  const matchedSortedEntries = Array.from(matchedSorted, ({ name, aliases, module }) =>
    makeModuleMethod(`${module ?? 'local.Project'}.${name}`, { aliases: aliases ?? [] }),
  )
  const matchResults = Array.from(matchedSortedEntries, (entry) => filtering.filter(entry, []))
  // Checking matching entries
  function checkResult(entry: SuggestionEntry, result: Opt<MatchResult>) {
    expect(result, `Matching entry ${entry.definitionPath}`).not.toBeNull()
    expect(
      matchedText(
        'memberOf' in entry && entry.memberOf ? qnLastSegment(entry.memberOf) : '',
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
    const entry = {
      ...makeModuleMethod(`${module ?? 'local.Project'}.${name}`),
      aliases: aliases ?? [],
    }
    expect(filtering.filter(entry, []), entry.definitionPath).toBeNull()
  }
})
