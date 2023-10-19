import {
  Filtering,
  type MatchRange,
  type MatchResult,
} from '@/components/ComponentBrowser/filtering'
import { SuggestionDb } from '@/stores/suggestionDatabase'
import {
  SuggestionKind,
  type SuggestionEntry,
  type SuggestionId,
} from '@/stores/suggestionDatabase/entry'
import { compareOpt } from '@/util/compare'
import { isSome } from '@/util/opt'
import { qnIsTopElement, qnLastSegmentIndex } from '@/util/qualifiedName'

interface ComponentLabel {
  label: string
  matchedAlias?: string
  matchedRanges?: MatchRange[]
}

export interface Component extends ComponentLabel {
  suggestionId: SuggestionId
  icon: string
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
    if (!match.memberOfRanges && !match.definedInRanges && !match.nameRanges)
      return { label: `${parentModule}.${entry.name}` }
    return {
      label: `${parentModule}.${entry.name}`,
      matchedRanges: [
        ...(match.memberOfRanges ?? match.definedInRanges ?? []).flatMap((range) =>
          range.end <= lastSegmentStart
            ? []
            : [
                {
                  start: Math.max(0, range.start - lastSegmentStart),
                  end: range.end - lastSegmentStart,
                },
              ],
        ),
        ...(match.nameRanges ?? []).map((range) => ({
          start: range.start + nameOffset,
          end: range.end + nameOffset,
        })),
      ],
    }
  } else
    return match.nameRanges
      ? { label: entry.name, matchedRanges: match.nameRanges }
      : { label: entry.name }
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

export function makeComponentList(db: SuggestionDb, filtering: Filtering): Component[] {
  function* matchSuggestions() {
    for (const [id, entry] of db.entries()) {
      const match = filtering.filter(entry)
      if (isSome(match)) {
        yield { id, entry, match }
      }
    }
  }
  const matched: MatchedSuggestion[] = Array.from(matchSuggestions())
  matched.sort(compareSuggestions)
  return Array.from(matched, ({ id, entry, match }): Component => {
    return {
      ...labelOfEntry(entry, filtering, match),
      suggestionId: id,
      icon: entry.iconName ?? 'marketplace',
      group: entry.groupIndex,
    }
  })
}
