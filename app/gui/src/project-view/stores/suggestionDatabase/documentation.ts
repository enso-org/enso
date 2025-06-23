import { extractMetadata } from '@/components/ComponentHelp/metadata'
import type { GroupInfo } from '@/stores/suggestionDatabase'
import { findIndexOpt } from '@/util/data/array'
import { isSome, type Opt } from '@/util/data/opt'
import { parseDocs, type Doc } from '@/util/docParser'
import { isIconName, type Icon } from '@/util/iconMetadata/iconName'
import { type QualifiedName } from '@/util/qualifiedName'
import { type DeepReadonly } from 'vue'
import { prerenderMarkdown } from 'ydoc-shared/ast/documentation'
import { ensoStandardMarkdownParser } from 'ydoc-shared/ast/ensoMarkdown'
import { unwrapOrWithLog } from 'ydoc-shared/util/data/result'

export interface DocumentationData {
  rawDocumentation: string
  isMarkdownDocs: boolean
  documentation: Doc.Section[]
  docSummaryHtml: string | undefined
  aliasesAndMacros: string[]
  macros: Record<string, string>
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
  const markdown = ensoStandardMarkdownParser.parse(documentation ?? '')
  const cursor = markdown.cursor()
  const metadataResult = extractMetadata(documentation ?? '', cursor.node)
  const metadata = unwrapOrWithLog(metadataResult, null, 'Invalid documentation metadata')

  const parsed = documentation != null ? parseDocs(documentation) : []

  const legacy = getLegacyProperties(parsed)

  const iconName = metadata?.icon ?? legacy.iconName
  const groupName = metadata?.group ?? legacy.groupName
  const aliases = metadata?.aliases ?? legacy.aliases
  const macros = metadata?.macros ?? legacy.macros
  const isPrivate = metadata?.private ?? legacy.isPrivate
  const isUnstable = metadata?.unstable ?? legacy.isUnstable
  const suggestedRank = metadata?.suggested ?? legacy.suggestedRank

  const groupIndex = groupName && project ? getGroupIndex(groupName, project, groups) : undefined

  return {
    rawDocumentation: prerenderMarkdown(documentation ?? ''),
    documentation: parsed,
    isMarkdownDocs: metadata != null,
    docSummaryHtml: getDocumentationSummary(parsed),
    iconName: iconName != null && isIconName(iconName) ? iconName : undefined,
    groupIndex,
    aliasesAndMacros: [...aliases, ...macros.map((macro) => macro.description)].sort(),
    macros: macros.reduce(
      (acc, macro) => {
        acc[macro.description] = macro.value
        return acc
      },
      {} as Record<string, string>,
    ),
    isPrivate: isPrivate,
    isUnstable: isUnstable,
    suggestedRank: suggestedRank,
  }
}

/**
 * These properties are extracted using legacy doc parser for old Enso-specific documentation format.
 * To be removed when fully migrated to Markdown docs.
 */
function getLegacyProperties(parsed: Doc.Section[]) {
  const groupName = tagValue(parsed, 'Group')
  const iconName = tagValue(parsed, 'Icon')

  const macroFilter = isTagNamed('Macro')
  const macros = parsed.filter(macroFilter).flatMap((section) => {
    const body = section.Tag.body
    const match = body.match(/^(\S+) (.+)$/)
    if (match) {
      const description = match[1]
      const value = match[2]
      if (description && value) {
        return [{ description, value }]
      }
    }
    return []
  })

  const aliases =
    tagValue(parsed, 'Alias')
      ?.trim()
      .split(/\s*,\s*/g) ?? []

  const isPrivate = isSome(tagValue(parsed, 'Private'))
  const isUnstable = isSome(tagValue(parsed, 'Unstable')) || isSome(tagValue(parsed, 'Advanced'))
  const suggestedRank = getSuggestedRank(parsed)

  return {
    groupName,
    iconName,
    macros,
    aliases,
    isPrivate,
    isUnstable,
    suggestedRank,
  }
}
