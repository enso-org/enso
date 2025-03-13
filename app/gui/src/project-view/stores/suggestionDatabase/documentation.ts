import type { GroupInfo } from '@/stores/suggestionDatabase'
import { findIndexOpt } from '@/util/data/array'
import { isSome, type Opt } from '@/util/data/opt'
import { parseDocs, type Doc } from '@/util/docParser'
import type { Icon } from '@/util/iconMetadata/iconName'
import { type QualifiedName } from '@/util/qualifiedName'
import { type DeepReadonly } from 'vue'

export interface DocumentationData {
  documentation: Doc.Section[]
  docSummaryHtml: string | undefined
  aliases: string[]
  /** A name of a custom icon to use when displaying the entry. */
  iconName: Icon | undefined
  /** An index of a group from group list in suggestionDb store this entry belongs to. */
  groupIndex: number | undefined
  /** If defined, it's a rank in "suggested" group (lower rank goes first) */
  suggestedRank: number | undefined
  isPrivate: boolean
  isUnstable: boolean
}

function isTagNamed(tag: string) {
  return (section: Doc.Section): section is { Tag: Doc.Section.Tag } => {
    return 'Tag' in section && section.Tag.tag == tag
  }
}

/** @internal */
export function tagValue(doc: Doc.Section[], tag: string): string | undefined {
  const tagSection = doc.find(isTagNamed(tag))
  return tagSection?.Tag.body
}

/** @internal */
export function getGroupIndex(
  groupName: string,
  project: QualifiedName,
  groups: DeepReadonly<GroupInfo[]>,
): number | undefined {
  let normalized: string
  if (groupName.indexOf('.') >= 0) {
    normalized = groupName
  } else {
    normalized = `${project}.${groupName}`
  }
  const index = findIndexOpt(groups, (group) => `${group.project}.${group.name}` == normalized)
  return index == null ? undefined : index
}

/** @internal */
export function getDocumentationSummary(sections: Doc.Section[]) {
  const firstParagraph = sections.find(
    (section): section is { Paragraph: Doc.Section.Paragraph } => 'Paragraph' in section,
  )?.Paragraph.body
  if (firstParagraph == null) return undefined
  const endOfSummary = firstParagraph.search(/<\s*p|(?<=\.)\W/)
  if (endOfSummary < 0) return firstParagraph
  else return firstParagraph.substring(0, endOfSummary)
}

/** @internal */
export function getSuggestedRank(sections: Doc.Section[]): number | undefined {
  const str = tagValue(sections, 'Suggested')
  if (str == null) return
  const rank = parseFloat(str)
  // Rank which is not a number is placed last.
  if (isNaN(rank)) return Infinity
  return rank
}

/** Retrieve {@link DocumentationData } from raw entry's documentation. */
export function documentationData(
  documentation: Opt<string>,
  project: QualifiedName | undefined,
  groups: DeepReadonly<GroupInfo[]>,
): DocumentationData {
  const parsed = documentation != null ? parseDocs(documentation) : []
  const groupName = tagValue(parsed, 'Group')
  const groupIndex = groupName && project ? getGroupIndex(groupName, project, groups) : undefined
  const iconName = tagValue(parsed, 'Icon')

  return {
    documentation: parsed,
    docSummaryHtml: getDocumentationSummary(parsed),
    iconName: iconName != null ? (iconName as Icon) : undefined,
    groupIndex,
    aliases:
      tagValue(parsed, 'Alias')
        ?.trim()
        .split(/\s*,\s*/g) ?? [],
    isPrivate: isSome(tagValue(parsed, 'Private')),
    isUnstable: isSome(tagValue(parsed, 'Unstable')) || isSome(tagValue(parsed, 'Advanced')),
    suggestedRank: getSuggestedRank(parsed),
  }
}

/**
 * Get the ICON tag value from the documentation block. Only use this function
 * if all you need is icon, since the docs parsing is an expensive operation.
 * @param documentation String representation of documentation block.
 * @returns Value of icon tag within the docs.
 */
export function getDocsIcon(documentation: Opt<string>): Opt<Icon> {
  const parsed = documentation != null ? parseDocs(documentation) : []
  return tagValue(parsed, 'Icon') as Opt<Icon>
}
