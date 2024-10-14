import type { Group } from '@/stores/suggestionDatabase'
import { findIndexOpt } from '@/util/data/array'
import { isSome, type Opt } from '@/util/data/opt'
import { parseDocs, type Doc } from '@/util/docParser'
import type { Icon } from '@/util/iconName'
import { type QualifiedName } from '@/util/qualifiedName'

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

/** @internal */
export function tagValue(doc: Doc.Section[], tag: string): Opt<string> {
  const tagSection = doc.find(isTagNamed(tag))
  if (tagSection == null) return null
  return tagSection.Tag.body
}

/** @internal */
export function getGroupIndex(
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

/** TODO: Add docs */
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
    aliases:
      tagValue(parsed, 'Alias')
        ?.trim()
        .split(/\s*,\s*/g) ?? [],
    isPrivate: isSome(tagValue(parsed, 'Private')),
    isUnstable: isSome(tagValue(parsed, 'Unstable')) || isSome(tagValue(parsed, 'Advanced')),
  }
}
