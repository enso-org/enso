import { getGroupIndex, tagValue } from '@/stores/suggestionDatabase/documentation'
import { unwrap } from '@/util/data/result'
import { parseDocs } from '@/util/docParser'
import { tryQualifiedName } from '@/util/qualifiedName'
import { expect, test } from 'vitest'

test.each([
  ['ALIAS Bar', 'Bar'],
  ['Some one section\n   But not tags here', null],
  ['GROUP different tag', null],
  ['PRIVATE\nGROUP Input\nALIAS Foo\n\nSeveral tags', 'Foo'],
])('Getting tag from docs case %#.', (doc, expected) => {
  const sections = parseDocs(doc)
  expect(tagValue(sections, 'Alias')).toBe(expected)
})

const groups = [
  { name: 'From Base', project: unwrap(tryQualifiedName('Standard.Base')) },
  { name: 'Other', project: unwrap(tryQualifiedName('local.Project')) },
  { name: 'Another', project: unwrap(tryQualifiedName('local.Project')) },
]
test.each([
  ['From Base', 'local.Project.Main', null],
  ['From Base', 'Standard.Base', 0],
  ['Standard.Base.From Base', 'local.Project.Main', 0],
  ['Other', 'local.Project.Main', 1],
  ['local.Project.Other', 'local.Project.Main', 1],
  ['Other', 'local.Project.Some.Deep.Submodule', 1],
  ['Another', 'local.Project.Main', 2],
  ['Not Existing', 'local.Project.Main', null],
])('Get group index case %#.', (name, definedIn, expected) => {
  const definedInQn = unwrap(tryQualifiedName(definedIn))
  expect(getGroupIndex(name, definedInQn, groups)).toBe(expected)
})
