import { mockProjectNameStore } from '@/stores/projectNames'
import { getGroupIndex, tagValue } from '@/stores/suggestionDatabase/documentation'
import { unwrap } from '@/util/data/result'
import { parseDocs } from '@/util/docParser'
import { parseAbsoluteProjectPath } from '@/util/projectPath'
import { tryQualifiedName, type QualifiedName } from '@/util/qualifiedName'
import { expect, test } from 'vitest'

test.each([
  ['ALIAS Bar', 'Bar'],
  ['Some one section\n   But not tags here', undefined],
  ['GROUP different tag', undefined],
  ['PRIVATE\nGROUP Input\nALIAS Foo\n\nSeveral tags', 'Foo'],
])('Getting tag from docs case %#.', (doc, expected) => {
  const sections = parseDocs(doc)
  expect(tagValue(sections, 'Alias')).toBe(expected)
})

const projectNames = mockProjectNameStore('local', 'Project')

const groups = [
  { name: 'From Base', project: 'Standard.Base' },
  { name: 'Other', project: 'local.Project' },
  { name: 'Another', project: 'local.Project' },
].map(({ name, project }) => ({
  name,
  project: parseAbsoluteProjectPath(unwrap(tryQualifiedName(project))).project as QualifiedName,
}))
test.each([
  ['From Base', 'local.Project.Main', undefined],
  ['From Base', 'Standard.Base', 0],
  ['Standard.Base.From Base', 'local.Project.Main', 0],
  ['Other', 'local.Project.Main', 1],
  ['local.Project.Other', 'local.Project.Main', 1],
  ['Other', 'local.Project.Some.Deep.Submodule', 1],
  ['Another', 'local.Project.Main', 2],
  ['Not Existing', 'local.Project.Main', undefined],
])('Get group index case %#.', (name, definedIn, expected) => {
  const definedInQn =
    projectNames.parseProjectPath(unwrap(tryQualifiedName(definedIn))).project ??
    ('local.Project' as QualifiedName)
  expect(getGroupIndex(name, definedInQn, groups)).toBe(expected)
})
