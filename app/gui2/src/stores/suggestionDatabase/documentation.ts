import type { Group } from '@/stores/suggestionDatabase'
import { findIndexOpt } from '@/util/array'
import { parseDocs, type Doc } from '@/util/docParser'
import type { Icon } from '@/util/iconName'
import { isSome, type Opt } from '@/util/opt'
import { tryQualifiedName, type QualifiedName } from '@/util/qualifiedName'
import { unwrap } from '@/util/result'

export interface DocumentationData {
  documentation: Doc.Section[]
  aliases: string[]
  iconName?: Icon
  groupIndex?: number
  isPrivate: boolean
  isUnstable: boolean
}

function isTagNamed(tag: string) {
  return (section: Doc.Section): section is { Tag: Doc.Section.Tag } => {
    return 'Tag' in section ? section.Tag.tag == tag : false
  }
}

function tagValue(doc: Doc.Section[], tag: string): Opt<string> {
  const tagSection = doc.find(isTagNamed(tag))
  if (tagSection == null) return null
  return tagSection.Tag.body
}

function getGroupIndex(
  groupName: string,
  entryModule: QualifiedName,
  groups: Group[],
): Opt<number> {
  let normalized: string
  if (groupName.indexOf('.') >= 0) {
    normalized = groupName
  } else {
    const project = /^[^.]+\.[^.]+/.exec(entryModule)
    if (project == null) return null
    normalized = `${project}.${groupName}`
  }
  return findIndexOpt(groups, (group) => `${group.project}.${group.name}` == normalized)
}

export function documentationData(
  documentation: Opt<string>,
  definedIn: QualifiedName,
  groups: Group[],
): DocumentationData {
  const parsed = documentation != null ? parseDocs(documentation) : []
  const groupName = tagValue(parsed, 'Group')
  const groupIndex = groupName ? getGroupIndex(groupName, definedIn, groups) : null
  const iconName = tagValue(parsed, 'Icon') as Opt<Icon>

  return {
    documentation: parsed,
    ...(iconName != null ? { iconName } : {}),
    ...(groupIndex != null ? { groupIndex } : {}),
    aliases: tagValue(parsed, 'Alias')?.split(',') ?? [],
    isPrivate: isSome(tagValue(parsed, 'Private')),
    isUnstable: isSome(tagValue(parsed, 'Unstable')) || isSome(tagValue(parsed, 'Advanced')),
  }
}

if (import.meta.vitest) {
  const { test, expect } = import.meta.vitest

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
}
