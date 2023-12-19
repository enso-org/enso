import { expect, test } from 'vitest'

import {
  compareSuggestions,
  labelOfEntry,
  makeComponent,
  type Component,
  type MatchedSuggestion,
} from '@/components/ComponentBrowser/component'
import { Filtering } from '@/components/ComponentBrowser/filtering'
import {
  makeCon,
  makeMethod,
  makeModule,
  makeModuleMethod,
  makeStaticMethod,
  type SuggestionEntry,
} from '@/stores/suggestionDatabase/entry'
import { allRanges } from '@/util/data/range'
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
  expect(labelOfEntry(suggestion, filteredView, { score: 0 }).label).toBe(expected)
  expect(labelOfEntry(suggestion, mainView, { score: 0 }).label).toBe(mainExpected ?? expected)
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

test('Matched ranges are correct', () => {
  function replaceMatches(component: Component) {
    if (!component.matchedRanges || component.matchedAlias) return component.label
    const parts: string[] = []
    for (const range of allRanges(component.matchedRanges, component.label.length)) {
      const text = component.label.slice(range.start, range.end)
      parts.push(range.isMatch ? `<${text}>` : text)
    }
    return parts.join('')
  }

  function replaceAliasMatches(component: Component) {
    if (!component.matchedRanges || !component.matchedAlias) return
    const parts: string[] = []
    for (const range of allRanges(component.matchedRanges, component.matchedAlias.length)) {
      const text = component.matchedAlias.slice(range.start, range.end)
      parts.push(range.isMatch ? `<${text}>` : text)
    }
    return parts.join('')
  }

  const pattern = 'foo_bar'
  const filtering = new Filtering({ pattern })
  const matchedSorted = [
    { name: 'foo_bar', highlighted: 'Project.<foo><_bar>' }, // exact match
    { name: 'foo_xyz_barabc', highlighted: 'Project.<foo>_xyz<_bar>abc' }, // first word exact match
    { name: 'fooabc_barabc', highlighted: 'Project.<foo>abc<_bar>abc' }, // first word match
    {
      name: 'bar',
      aliases: ['foo_bar', 'foo'],
      highlighted: 'Project.bar',
      highlightedAlias: '<foo><_bar>',
    }, // exact alias match
    {
      name: 'bar',
      aliases: ['foo', 'foo_xyz_barabc'],
      highlighted: 'Project.bar',
      highlightedAlias: '<foo>_xyz<_bar>abc',
    }, // alias first word exact match
    {
      name: 'bar',
      aliases: ['foo', 'fooabc_barabc'],
      highlighted: 'Project.bar',
      highlightedAlias: '<foo>abc<_bar>abc',
    }, // alias first word match
    { name: 'xyz_foo_abc_bar_xyz', highlighted: 'Project.xyz_<foo>_abc<_bar>_xyz' }, // exact word match
    { name: 'xyz_fooabc_abc_barabc_xyz', highlighted: 'Project.xyz_<foo>abc_abc<_bar>abc_xyz' }, // non-exact word match
    {
      name: 'bar',
      aliases: ['xyz_foo_abc_bar_xyz'],
      highlighted: 'Project.bar',
      highlightedAlias: 'xyz_<foo>_abc<_bar>_xyz',
    }, // alias word exact match
    {
      name: 'bar',
      aliases: ['xyz_fooabc_abc_barabc_xyz'],
      highlighted: 'Project.bar',
      highlightedAlias: 'xyz_<foo>abc_abc<_bar>abc_xyz',
    }, // alias word start match
  ]
  const entries = Array.from(matchedSorted, ({ name, aliases }, id) => {
    const entry: SuggestionEntry = {
      ...makeModuleMethod(`local.Project.${name}`),
      aliases: aliases ?? [],
    }
    return { id, entry, match: filtering.filter(entry)! }
  })
  for (let i = 0; i < entries.length; i += 1) {
    expect(
      replaceMatches(makeComponent(entries[i]!, filtering)),
      `replaceMatches(${JSON.stringify(matchedSorted[i])})`,
    ).toEqual(matchedSorted[i]!.highlighted)
    expect(
      replaceAliasMatches(makeComponent(entries[i]!, filtering)),
      `replaceAliasMatches(${JSON.stringify(matchedSorted[i])})`,
    ).toEqual(matchedSorted[i]!.highlightedAlias)
  }
})
