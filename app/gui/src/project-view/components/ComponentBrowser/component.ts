import { Filtering, type MatchResult } from '@/components/ComponentBrowser/filtering'
import { SuggestionDb } from '@/stores/suggestionDatabase'
import {
  entryDisplayPath,
  entryIsStatic,
  SuggestionKind,
  type SuggestionEntry,
  type SuggestionId,
} from '@/stores/suggestionDatabase/entry'
import { compareOpt } from '@/util/compare'
import { isSome } from '@/util/data/opt'
import { displayedIconOf } from '@/util/getIconName'
import { type Icon } from '@/util/iconMetadata/iconName'
import { type ProjectPath } from '@/util/projectPath'
import * as map from 'lib0/map'
import { Range } from 'ydoc-shared/util/data/range'

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
  suggestionId?: SuggestionId
  icon: Icon
  group?: number | undefined
}

export interface SuggestedComponent extends Component {
  rank: number
}

/** @returns the displayed label of given suggestion entry with information of highlighted ranges. */
export function labelOfEntry(entry: SuggestionEntry, match: MatchResult): ComponentLabelInfo {
  if (entryIsStatic(entry)) {
    const label = entryDisplayPath(entry)
    if ((!match.ownerNameRanges && !match.nameRanges) || match.matchedAlias) {
      return {
        label,
        matchedAlias: match.matchedAlias,
        matchedRanges: match.nameRanges,
      }
    }
    const nameOffset = label.length - entry.name.length
    return {
      label,
      matchedAlias: match.matchedAlias,
      matchedRanges: [
        ...(match.ownerNameRanges ?? []),
        ...(match.nameRanges ?? []).map((range) => range.shift(nameOffset)),
      ],
    }
  } else
    return match.nameRanges ?
        { label: entry.name, matchedAlias: match.matchedAlias, matchedRanges: match.nameRanges }
      : { label: entry.name, matchedAlias: match.matchedAlias }
}

function formatLabel(labelInfo: ComponentLabelInfo): ComponentLabel {
  const shift = labelInfo.label.length + 2
  const shiftRange = (range: Range) => range.shift(shift)
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
  const moduleCompare =
    (a.entry.definedIn.project ?? '').localeCompare(b.entry.definedIn.project ?? '') ||
    (a.entry.definedIn.path ?? '').localeCompare(b.entry.definedIn.path ?? '')
  if (moduleCompare !== 0) return moduleCompare
  return a.id - b.id
}

/** Create {@link Component} from information about suggestion and matching. */
export function makeComponent({ id, entry, match }: MatchedSuggestion): Component {
  return {
    ...formatLabel(labelOfEntry(entry, match)),
    suggestionId: id,
    icon: displayedIconOf(entry),
    group: entry.groupIndex,
  }
}

/**
 * A component group identifier: an index in suggestion database's group array, or one
 * of the special groups.
 */
export type GroupId = 'all' | 'suggestions' | number

/** Create {@link Component} list for each displayed group from filtered suggestions. */
export function makeComponentLists(
  db: SuggestionDb,
  filtering: Filtering,
): Map<GroupId, ReadonlyArray<Component>> {
  function* matchSuggestions() {
    const additionalSelfTypes: ProjectPath[] = []
    if (filtering.selfArg?.type === 'known') {
      const entry = db.getEntryByProjectPath(filtering.selfArg.typename)
      if (entry) additionalSelfTypes.push(...db.ancestors(entry))
    }

    for (const [id, entry] of db.entries()) {
      if (!entry) continue
      const match = filtering.filter(entry, additionalSelfTypes)
      if (isSome(match)) {
        const component = makeComponent({ id, entry, match })
        yield { id, entry, match, component }
      }
    }
  }
  const matched = Array.from(matchSuggestions()).sort(compareSuggestions)
  const groups = new Map<GroupId, Component[]>()
  const suggested: SuggestedComponent[] = []
  if (filtering.pattern == null) {
    for (const { entry, component } of matched) {
      if (entry.suggestedRank != null) {
        suggested.push({ rank: entry.suggestedRank, ...component })
      }
    }
  }
  if (suggested.length > 0) {
    suggested.sort((a, b) => a.rank - b.rank)
    groups.set('suggestions', suggested)
  } else {
    groups.set(
      'all',
      Array.from(matched, ({ component }) => component),
    )
  }

  for (const { entry, component } of matched) {
    if (entry.groupIndex != null) {
      map.setIfUndefined(groups, entry.groupIndex, (): Component[] => []).push(component)
    }
  }

  return groups
}
