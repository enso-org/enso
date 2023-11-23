import { Filtering, type MatchResult } from '@/components/ComponentBrowser/filtering'
import { SuggestionDb } from '@/stores/suggestionDatabase'
import {
  SuggestionKind,
  type SuggestionEntry,
  type SuggestionId,
} from '@/stores/suggestionDatabase/entry'
import { compareOpt } from '@/util/compare'
import { displayedIconOf } from '@/util/getIconName'
import type { Icon } from '@/util/iconName'
import { isSome } from '@/util/opt'
import { qnIsTopElement, qnLastSegmentIndex } from '@/util/qualifiedName'
import { Range } from '@/util/range'

interface ComponentLabel {
  label: string
  matchedAlias?: string | undefined
  matchedRanges?: Range[] | undefined
}

export interface Component extends ComponentLabel {
  suggestionId: SuggestionId
  icon: Icon
  group?: number | undefined
}

export function labelOfEntry(
  entry: SuggestionEntry,
  filtering: Filtering,
  match: MatchResult,
): ComponentLabel {
  const isTopModule = entry.kind == SuggestionKind.Module && qnIsTopElement(entry.definedIn)
  if (filtering.isMainView() && isTopModule) return { label: entry.definedIn }
  else if (entry.memberOf && entry.selfType == null) {
    const lastSegmentStart = qnLastSegmentIndex(entry.memberOf) + 1
    const parentModule = entry.memberOf.substring(lastSegmentStart)
    const nameOffset = parentModule.length + 1
    if (
      (!match.memberOfRanges && !match.definedInRanges && !match.nameRanges) ||
      match.matchedAlias
    )
      return {
        label: `${parentModule}.${entry.name}`,
        matchedAlias: match.matchedAlias,
        matchedRanges: match.nameRanges,
      }
    return {
      label: `${parentModule}.${entry.name}`,
      matchedAlias: match.matchedAlias,
      matchedRanges: [
        ...(match.memberOfRanges ?? match.definedInRanges ?? []).flatMap((range) =>
          range.end <= lastSegmentStart
            ? []
            : [
                new Range(
                  Math.max(0, range.start - lastSegmentStart),
                  range.end - lastSegmentStart,
                ),
              ],
        ),
        ...(match.nameRanges ?? []).map(
          (range) => new Range(range.start + nameOffset, range.end + nameOffset),
        ),
      ],
    }
  } else
    return match.nameRanges
      ? { label: entry.name, matchedAlias: match.matchedAlias, matchedRanges: match.nameRanges }
      : { label: entry.name, matchedAlias: match.matchedAlias }
}

export interface MatchedSuggestion {
  id: SuggestionId
  entry: SuggestionEntry
  match: MatchResult
}

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

export interface ComponentInfo {
  id: number
  entry: SuggestionEntry
  match: MatchResult
}

export function makeComponent(
  { id, entry, match }: ComponentInfo,
  filtering: Filtering,
): Component {
  return {
    ...labelOfEntry(entry, filtering, match),
    suggestionId: id,
    icon: displayedIconOf(entry),
    group: entry.groupIndex,
  }
}

export function makeComponentList(db: SuggestionDb, filtering: Filtering): Component[] {
  function* matchSuggestions() {
    for (const [id, entry] of db.entries()) {
      const match = filtering.filter(entry)
      if (isSome(match)) {
        yield { id, entry, match }
      }
    }
  }
  const matched = Array.from(matchSuggestions()).sort(compareSuggestions)
  return Array.from(matched, (info) => makeComponent(info, filtering))
}
