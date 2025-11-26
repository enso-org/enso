import type { GroupInfo } from '$/providers/openedProjects/suggestionDatabase'
import { extractMetadata } from '@/components/ComponentHelp/metadata'
import { findIndexOpt } from '@/util/data/array'
import type { Opt } from '@/util/data/opt'
import { isIconName, type Icon } from '@/util/iconMetadata/iconName'
import type { QualifiedName } from '@/util/qualifiedName'
import type { SyntaxNodeRef } from '@lezer/common'
import { unwrapOrWithLog } from 'enso-common/src/utilities/data/result'
import type { DeepReadonly } from 'vue'
import { prerenderMarkdown } from 'ydoc-shared/ast/documentation'
import { ensoMarkdownParser } from 'ydoc-shared/ast/ensoMarkdown'

export interface DocumentationData {
  documentation: string
  documentationSummary: string | undefined
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
export function getDocumentationSummary(parsed: SyntaxNodeRef, source: string) {
  const firstParagraph = parsed.node.getChild('Paragraph')
  if (firstParagraph == null) return undefined
  const paragraphText = source.slice(firstParagraph.from, firstParagraph.to)
  const endOfSummary = paragraphText.search(/(?<=\.)\W/)
  if (endOfSummary < 0) return paragraphText
  else return paragraphText.slice(0, endOfSummary)
}

/** Retrieve {@link DocumentationData } from raw entry's documentation. */
export function documentationData(
  documentation: Opt<string>,
  project: QualifiedName | undefined,
  groups: DeepReadonly<GroupInfo[]>,
): DocumentationData {
  const prerendered = prerenderMarkdown(documentation ?? '')
  const markdown = ensoMarkdownParser.parse(prerendered)
  const cursor = markdown.cursor()
  const summary = getDocumentationSummary(cursor.node, prerendered)
  const metadataResult = extractMetadata(documentation ?? '', cursor.node)
  const metadata = unwrapOrWithLog(metadataResult, null, 'Invalid documentation metadata')

  const iconName = metadata?.icon
  const groupName = metadata?.group
  const aliases = metadata?.aliases ?? []
  const macros = metadata?.macros ?? []
  const isPrivate = metadata?.private ?? false
  const isUnstable = metadata?.unstable ?? false
  const suggestedRank = metadata?.suggested

  const groupIndex = groupName && project ? getGroupIndex(groupName, project, groups) : undefined

  return {
    documentation: prerendered,
    documentationSummary: summary,
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
