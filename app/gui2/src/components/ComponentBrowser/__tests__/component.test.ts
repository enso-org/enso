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
  makeConstructor,
  makeMethod,
  makeModule,
  makeModuleMethod,
  makeStaticMethod,
} from '@/stores/suggestionDatabase/entry'
import { allRanges } from '@/util/data/range'
import shuffleSeed from 'shuffle-seed'

test.each([
  [makeModuleMethod('Standard.Base.Data.read'), 'Data.read'],
  [makeStaticMethod('Standard.Base.Data.Vector.new'), 'Vector.new'],
  [makeMethod('Standard.Base.Data.Vector.get'), 'get'],
  [makeConstructor('Standard.Table.Join_Kind.Join_Kind.Inner'), 'Join_Kind.Inner'],
  [makeModuleMethod('local.Project.main'), 'Project.main'],
])("$name Component's label is valid", (suggestion, expected) => {
  expect(labelOfEntry(suggestion, { score: 0 }).label).toBe(expected)
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

test.each`
  name                           | aliases                          | highlighted
  ${'foo_bar'}                   | ${[]}                            | ${'Project.<foo><_bar>'}
  ${'foo_xyz_barabc'}            | ${[]}                            | ${'Project.<foo>_xyz<_bar>abc'}
  ${'fooabc_barabc'}             | ${[]}                            | ${'Project.<foo>abc<_bar>abc'}
  ${'bar'}                       | ${['foo_bar', 'foo']}            | ${'Project.bar (<foo><_bar>)'}
  ${'bar'}                       | ${['foo', 'foo_xyz_barabc']}     | ${'Project.bar (<foo>_xyz<_bar>abc)'}
  ${'bar'}                       | ${['foo', 'fooabc_barabc']}      | ${'Project.bar (<foo>abc<_bar>abc)'}
  ${'xyz_foo_abc_bar_xyz'}       | ${[]}                            | ${'Project.xyz_<foo>_abc<_bar>_xyz'}
  ${'xyz_fooabc_abc_barabc_xyz'} | ${[]}                            | ${'Project.xyz_<foo>abc_abc<_bar>abc_xyz'}
  ${'bar'}                       | ${['xyz_foo_abc_bar_xyz']}       | ${'Project.bar (xyz_<foo>_abc<_bar>_xyz)'}
  ${'bar'}                       | ${['xyz_fooabc_abc_barabc_xyz']} | ${'Project.bar (xyz_<foo>abc_abc<_bar>abc_xyz)'}
`('Matched ranges of $highlighted are correct', ({ name, aliases, highlighted }) => {
  function replaceMatches(component: Component) {
    if (!component.matchedRanges) return component.label
    const parts: string[] = []
    for (const range of allRanges(component.matchedRanges, component.label.length)) {
      const text = component.label.slice(range.start, range.end)
      parts.push(range.isMatch ? `<${text}>` : text)
    }
    return parts.join('')
  }

  const pattern = 'foo_bar'
  const entry = {
    ...makeModuleMethod(`local.Project.${name}`),
    aliases: aliases ?? [],
  }
  const filtering = new Filtering({ pattern })
  const componentInfo = { id: 0, entry, match: filtering.filter(entry)! }
  expect(replaceMatches(makeComponent(componentInfo))).toEqual(highlighted)
})
