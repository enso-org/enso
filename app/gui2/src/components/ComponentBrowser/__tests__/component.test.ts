import { expect, test } from 'vitest'

import {
  compareSuggestions,
  labelOfEntry,
  type MatchedSuggestion,
} from '@/components/ComponentBrowser/component'
import { Filtering } from '@/components/ComponentBrowser/filtering'
import {
  makeCon,
  makeMethod,
  makeModule,
  makeModuleMethod,
  makeStaticMethod,
} from '@/stores/suggestionDatabase/entry'
import shuffleSeed from 'shuffle-seed'

test.each([
  [makeModuleMethod('Standard.Base.Data.read'), 'Data.read'],
  [makeStaticMethod('Standard.Base.Data.Vector.new'), 'Vector.new'],
  [makeMethod('Standard.Base.Data.Vector.get'), 'get'],
  [makeCon('Standard.Table.Data.Join_Kind.Join_Kind.LeftInner'), 'Join_Kind.LeftInner'],
  [makeModule('Standard.Table.Data.Join_Kind'), 'Join_Kind'],
  [makeModule('Standard.Table.Data'), 'Data', 'Standard.Table.Data'],
  [makeModuleMethod('local.Project.main'), 'Project.main'],
])("$name Component's label is valid", (suggestion, expected, mainExpected?) => {
  const mainView = new Filtering({})
  const filteredView = new Filtering({ pattern: 'e' })
  expect(labelOfEntry(suggestion, filteredView)).toBe(expected)
  expect(labelOfEntry(suggestion, mainView)).toBe(mainExpected ?? expected)
})

test('Suggestions are ordered properly', () => {
  const sortedEntries: MatchedSuggestion[] = [
    {
      id: 100,
      entry: makeModuleMethod('local.Project.Z.best_score'),
      match: { score: 0 },
    },
    {
      id: 90,
      entry: { ...makeModuleMethod('local.Project.Z.b'), groupIndex: 0 },
      match: { score: 50 },
    },
    {
      id: 91,
      entry: { ...makeModuleMethod('local.Project.Z.a'), groupIndex: 0 },
      match: { score: 50 },
    },
    {
      id: 89,
      entry: { ...makeModuleMethod('local.Project.A.foo'), groupIndex: 1 },
      match: { score: 50 },
    },
    {
      id: 88,
      entry: { ...makeModuleMethod('local.Project.B.another_module'), groupIndex: 1 },
      match: { score: 50 },
    },
    {
      id: 87,
      entry: { ...makeModule('local.Project.A'), groupIndex: 1 },
      match: { score: 50 },
    },
    {
      id: 50,
      entry: makeModuleMethod('local.Project.Z.module_content'),
      match: { score: 50 },
    },
    {
      id: 49,
      entry: makeModule('local.Project.Z.Module'),
      match: { score: 50 },
    },
  ]
  const expectedOrdering = Array.from(sortedEntries, (entry) => entry.id)
  for (let i = 100; i < 120; i++) {
    const entries = shuffleSeed.shuffle(sortedEntries, i)

    entries.sort(compareSuggestions)
    const result = Array.from(entries, (entry) => entry.id)
    expect(result).toStrictEqual(expectedOrdering)
  }
})
