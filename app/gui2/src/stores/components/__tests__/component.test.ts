import { expect, test } from 'vitest'

import {
  makeCon,
  makeMethod,
  makeModule,
  makeModuleMethod,
  makeStaticMethod,
} from '@/stores/suggestionDatabase/entry'
import { compareSuggestions, labelOfEntry, type MatchedSuggestion } from '@/stores/components'
import { Filtering } from '../filtering'
import shuffleSeed from 'shuffle-seed'

test.each([
  { suggestion: makeModuleMethod('Standard.Base.Data', 'read', 'Any'), expected: 'Data.read' },
  {
    suggestion: makeStaticMethod('Standard.Base.Data.Vector', 'new', 'Any'),
    expected: 'Vector.new',
  },
  { suggestion: makeMethod('Standard.Base.Data.Vector', 'get', 'Any'), expected: 'get' },
  {
    suggestion: makeCon('Standard.Table.Data.Join_Kind.Join_Kind', 'LeftInner'),
    expected: 'Join_Kind.LeftInner',
  },
  {
    suggestion: makeModule('Standard.Table.Data.Join_Kind'),
    expected: 'Join_Kind',
  },
  {
    suggestion: makeModule('Standard.Table.Data'),
    mainExpected: 'Standard.Table.Data',
    expected: 'Data',
  },
  { suggestion: makeModuleMethod('local.Project', 'main', 'Any'), expected: 'Project.main' },
])(
  "$suggestion.name Component's label is $expected",
  ({ suggestion, expected, mainExpected: mainExpected }) => {
    const mainView = new Filtering({})
    const filteredView = new Filtering({ pattern: 'e' })
    expect(labelOfEntry(suggestion, filteredView)).toBe(expected)
    expect(labelOfEntry(suggestion, mainView)).toBe(mainExpected ?? expected)
  },
)

test('Suggestions are ordered properly', () => {
  const sortedEntries: MatchedSuggestion[] = [
    {
      id: 100,
      entry: makeModuleMethod('local.Project.Z', 'best_score', 'Any'),
      match: { score: 0 },
    },
    {
      id: 90,
      entry: { ...makeModuleMethod('local.Project.Z', 'b', 'Any'), groupIndex: 0 },
      match: { score: 50 },
    },
    {
      id: 91,
      entry: { ...makeModuleMethod('local.Project.Z', 'a', 'Any'), groupIndex: 0 },
      match: { score: 50 },
    },
    {
      id: 89,
      entry: { ...makeModuleMethod('local.Project.A', 'foo', 'Any'), groupIndex: 1 },
      match: { score: 50 },
    },
    {
      id: 88,
      entry: { ...makeModuleMethod('local.Project.B', 'another_module', 'Any'), groupIndex: 1 },
      match: { score: 50 },
    },
    {
      id: 87,
      entry: { ...makeModule('local.Project.A'), groupIndex: 1 },
      match: { score: 50 },
    },
    {
      id: 50,
      entry: makeModuleMethod('local.Project.Z', 'module_content', 'Any'),
      match: { score: 50 },
    },
    {
      id: 49,
      entry: makeModule('local.Project.Z.Module'),
      match: { score: 50 },
    },
    {
      id: 1,
      entry: { ...makeModuleMethod('local.Project.A', 'a', 'Any'), groupIndex: 0 },
      match: null,
    },
  ]
  const expectedOrdering = Array.from(sortedEntries, (entry) => entry.id)
  for (let i = 100; i < 120; i++) {
    const entries = shuffleSeed.shuffle(sortedEntries, i)
    console.log(Array.from(entries, (entry) => entry.id))
    entries.sort(compareSuggestions)
    const result = Array.from(entries, (entry) => entry.id)
    expect(result).toStrictEqual(expectedOrdering)
  }
})
