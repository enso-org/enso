import { Filtering, type MatchResult } from '@/components/ComponentBrowser/filtering'
import { SuggestionDb } from '@/stores/suggestionDatabase'
import {
  SuggestionKind,
  type SuggestionEntry,
  type SuggestionId,
} from '@/stores/suggestionDatabase/entry'
import { compareOpt } from '@/util/compare'
import { isSome } from '@/util/data/opt'
import { Range } from '@/util/data/range'
import { ANY_TYPE_QN } from '@/util/ensoTypes'
import { displayedIconOf } from '@/util/getIconName'
import type { Icon } from '@/util/iconMetadata/iconName'
import type { QualifiedName } from '@/util/qualifiedName'
import { qnLastSegmentIndex, tryQualifiedName } from '@/util/qualifiedName'

interface ComponentLabelInfo {
  label: string
  matchedAlias?: string | undefined
  matchedRanges?: Range[] | undefined
}

interface ComponentLabel {
  label: string
  matchedRanges?: Range[] | undefined
}

/** A model of component suggestion displayed in the Component Browser. */
export interface Component extends ComponentLabel {
  suggestionId: SuggestionId
  icon: Icon
  group?: number | undefined
}

/** @returns the displayed label of given suggestion entry with information of highlighted ranges. */
export function labelOfEntry(entry: SuggestionEntry, match: MatchResult): ComponentLabelInfo {
  if (entry.memberOf && entry.selfType == null) {
    const ownerLastSegmentStart = qnLastSegmentIndex(entry.memberOf) + 1
    const ownerName = entry.memberOf.substring(ownerLastSegmentStart)
    const nameOffset = ownerName.length + '.'.length
    if ((!match.ownerNameRanges && !match.nameRanges) || match.matchedAlias) {
      return {
        label: `${ownerName}.${entry.name}`,
        matchedAlias: match.matchedAlias,
        matchedRanges: match.nameRanges,
      }
    }
    return {
      label: `${ownerName}.${entry.name}`,
      matchedAlias: match.matchedAlias,
      matchedRanges: [
        ...(match.ownerNameRanges ?? []),
        ...(match.nameRanges ?? []).map(
          (range) => new Range(range.start + nameOffset, range.end + nameOffset),
        ),
      ],
    }
  } else
    return match.nameRanges ?
        { label: entry.name, matchedAlias: match.matchedAlias, matchedRanges: match.nameRanges }
      : { label: entry.name, matchedAlias: match.matchedAlias }
}

function formatLabel(labelInfo: ComponentLabelInfo): ComponentLabel {
  const shift = labelInfo.label.length + 2
  const shiftRange = (range: Range) => new Range(range.start + shift, range.end + shift)
  return !labelInfo.matchedAlias ?
      { label: labelInfo.label, matchedRanges: labelInfo.matchedRanges }
    : {
        label: `${labelInfo.label} (${labelInfo.matchedAlias})`,
        matchedRanges: labelInfo.matchedRanges?.map(shiftRange),
      }
}

/** Suggestion entry with matching information. */
export interface MatchedSuggestion {
  id: SuggestionId
  entry: SuggestionEntry
  match: MatchResult
}

/** A suggestion comparator. The "lower" suggestion should be first in Component Browser's list. */
export function compareSuggestions(a: MatchedSuggestion, b: MatchedSuggestion): number {
  const matchCompare = a.match.score - b.match.score
  if (matchCompare !== 0) return matchCompare
  const groupCompare = compareOpt(a.entry.groupIndex, b.entry.groupIndex, 1)
  if (groupCompare !== 0) return groupCompare
  const kindCompare =
    +(a.entry.kind === SuggestionKind.Module) - +(b.entry.kind === SuggestionKind.Module)
  if (kindCompare !== 0) return kindCompare
  const moduleCompare = a.entry.definedIn.localeCompare(b.entry.definedIn)
  if (moduleCompare !== 0) return moduleCompare
  return a.id - b.id
}

interface ComponentInfo {
  id: number
  entry: SuggestionEntry
  match: MatchResult
}

/** Create {@link Component} from information about suggestion and matching. */
export function makeComponent({ id, entry, match }: ComponentInfo): Component {
  return {
    ...formatLabel(labelOfEntry(entry, match)),
    suggestionId: id,
    icon: displayedIconOf(entry),
    group: entry.groupIndex,
  }
}

/** Create {@link Component} list from filtered suggestions. */
export function makeComponentList(db: SuggestionDb, filtering: Filtering): Component[] {
  function* matchSuggestions() {
    // All types are descendants of `Any`, so we can safely prepopulate it here.
    // This way, we will use it even when `selfArg` is not a valid qualified name.
    const additionalSelfTypes: QualifiedName[] = [ANY_TYPE_QN]
    if (filtering.selfArg?.type === 'known') {
      const maybeName = tryQualifiedName(filtering.selfArg.typename)
      if (maybeName.ok) populateAdditionalSelfTypes(db, additionalSelfTypes, maybeName.value)
    }

    for (const [id, entry] of db.entries()) {
      const match = filtering.filter(entry, additionalSelfTypes)
      if (isSome(match)) {
        yield { id, entry, match }
      }
    }
  }
  const matched = Array.from(matchSuggestions()).sort(compareSuggestions)
  return Array.from(matched, (info) => makeComponent(info))
}

/**
 * Type can inherit methods from `parentType`, and it can do that recursively.
 * In practice, these hierarchies are at most two levels deep.
 */
function populateAdditionalSelfTypes(db: SuggestionDb, list: QualifiedName[], name: QualifiedName) {
  let entry = db.getEntryByQualifiedName(name)
  // We don’t need to add `Any` to the list, because the caller already did that.
  while (entry != null && entry.parentType != null && entry.parentType !== ANY_TYPE_QN) {
    list.push(entry.parentType)
    entry = db.getEntryByQualifiedName(entry.parentType)
  }
}
