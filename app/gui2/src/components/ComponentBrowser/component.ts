import {
  SuggestionKind,
  type SuggestionEntry,
  type SuggestionId,
} from '@/stores/suggestionDatabase/entry'
import { SuggestionDb } from '@/stores/suggestionDatabase'
import { Filtering, type MatchResult } from './filtering'
import { qnIsTopElement, qnLastSegment } from '@/util/qualifiedName'
import { compareOpt } from '@/util/compare'
import { isSome } from '@/util/opt'

export interface Component {
  suggestionId: SuggestionId
  icon: string
  label: string
  match: MatchResult
  group?: number
}

export function labelOfEntry(entry: SuggestionEntry, filtering: Filtering) {
  const isTopModule = entry.kind == SuggestionKind.Module && qnIsTopElement(entry.definedIn)
  if (filtering.isMainView() && isTopModule) return entry.definedIn
  else if (entry.memberOf && entry.selfType == null)
    return `${qnLastSegment(entry.memberOf)}.${entry.name}`
  else return entry.name
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
  return Array.from(matched, ({ id, entry, match }) => {
    return {
      suggestionId: id,
      icon: entry.iconName ?? 'marketplace',
      label: labelOfEntry(entry, filtering),
      match,
      group: entry.groupIndex,
    }
  })
}
