import { computed, ref, type ComputedRef } from 'vue'
import { defineStore } from 'pinia'
import type { QualifiedName, SuggestionEntry } from './suggestionDatabase/entry'
import { useSuggestionDbStore } from './suggestionDatabase'

export enum MatchTypeScore {
  NameStartsWith = 0,
  TypeStartsWith = 500,
  AliasStartsWith = 1000,
  NameWordMatch = 2000,
  AliasWordMatch = 3000,
  NameInitialMatch = 4000,
  AliasInitialMatch = 5000,
}

function scoreMatch(matchType: MatchTypeScore, match: string, search: string): number {
  let matchScore = match === search ? 0 : 50
  if (search !== '') {
    //TODO[ao] understand and optimize this:
    const pattern = new RegExp('(?:^|_)(' + search.replaceAll('_', '[^_]*).*?_(') + '[^_]*).*')
    const matched = pattern.exec(match) ?? ['']
    matched.shift()
    const match_text = matched.join('_')
    matchScore += Math.floor(((match_text.length - search.length) * 50) / match_text.length)
  }

  return (matchType + matchScore) * 100
}

function scoreMatch2(match, search) {
  let matchScore = match === search ? 0 : 50
  if (search !== '') {
    //TODO[ao] understand and optimize this:
    const pattern = new RegExp('(?:^|_)(' + search.replaceAll('_', '[^_]*).*?_(') + '[^_]*).*')
    const matched = pattern.exec(match) ?? ['']
    matched.shift()
    const match_text = matched.join('_')
    console.log(match_text)
    return Math.floor(((match_text.length - search.length) * 50) / match_text.length)
  }

  return matchScore
}

export class Filtering {
  pattern: string
  wordMatchRegex?: RegExp
  initialsMatchRegex?: RegExp
  selfType?: QualifiedName
  qualifiedNameRegex?: RegExp
  showUnstable: boolean = false
  showLocal: boolean = false

  constructor(
    pattern: string,
    options: {
      selfType?: QualifiedName
      qualifiedNamePattern?: QualifiedName
      showUnstable?: boolean
      showLocal?: boolean
    },
  ) {
    const { selfType, qualifiedNamePattern, showUnstable, showLocal } = options
    // TODO[ao] Add comment here about the groups. Or elsewhere
    this.pattern = pattern
    // This switch has deliberate fallthroughs.
    switch (pattern.length) {
      default:
        this.initialsMatchRegex = new RegExp('(^|_)' + pattern.split('').join('.*_'), 'i')
      case 1:
        this.wordMatchRegex = new RegExp('(?:^|_)(' + pattern.replaceAll('_', '[^_]*).*?_(') + '[^_]*).*')
      case 0:
        break
    }
    
    this.selfType = selfType
    if (qualifiedNamePattern != null) {
      this.qualifiedNameRegex = new RegExp(
        '(^|\\.)' + qualifiedNamePattern.replaceAll('.', '.*\\.'),
        'i',
      )
    }
    this.showUnstable = showUnstable ?? false
    this.showLocal = showLocal ?? false
  }

  selfTypeMatches(entry: SuggestionEntry): boolean {
    if (this.selfType == null) {
      return entry.selfType == null
    } else {
      return entry.selfType === this.selfType
    }
  }

  qualifiedNameMatches(entry: SuggestionEntry): boolean {
    if (this.qualifiedNameRegex == null) return true
    const entryQn = entry.memberOf ?? entry.definedIn
    return this.qualifiedNameRegex.test(entryQn)
  }

  tryMatch(entry: SuggestionEntry): { matchedString: string; score: MatchTypeScore } | null {
    if (!this.selfTypeMatches(entry) || !this.qualifiedNameMatches(entry)) return null
    if (this.pattern == '') {
      return { matchedString: entry.name, score: 0 }
    }
    
    if (nameMatch?.index === 0) {
      return { matchedString: entry.name, typeScore: MatchTypeScore.NameStartsWith }
    }
    const matchedAlias = entry.aliases.reduce<{ alias: string; index: number } | null>(
      (best, alias) => {
        const index = this.wordMatchRegex.exec(alias)?.index
        if (index != null && (best == null || best.index > index)) {
          return { alias, index }
        } else {
          return best
        }
      },
      null,
    )
    if (matchedAlias?.index === 0) {
      return { matchedString: matchedAlias.alias, typeScore: MatchTypeScore.AliasStartsWith }
    }
    if (nameMatchIndex) {
      return { matchedString: entry.name, typeScore: MatchTypeScore.NameWordMatch }
    }
    if (matchedAlias) {
      return { matchedString: matchedAlias.alias, typeScore: MatchTypeScore.AliasWordMatch }
    }
    if (this.initialsMatchRegex.test(entry.name)) {
      return { matchedString: entry.name, typeScore: MatchTypeScore.NameInitialMatch }
    }
    const matchedAliasInitials = entry.aliases.find((alias) => this.initialsMatchRegex.test(alias))
    if (matchedAliasInitials) {
      return { matchedString: matchedAliasInitials, typeScore: MatchTypeScore.AliasInitialMatch }
    }
    return null
  }

  score(entry: SuggestionEntry): { matchedString: string; score: number } | null {
    const match = this.tryMatch(entry)
    if (match == null) return null
    let matchScore = match === search ? 0 : 50
      if (search !== '') {
        //TODO[ao] understand and optimize this:
        const pattern = new RegExp('(?:^|_)(' + search.replaceAll('_', '[^_]*).*?_(') + '[^_]*).*')
        const matched = pattern.exec(match) ?? ['']
        matched.shift()
        const match_text = matched.join('_')
        matchScore += Math.floor(((match_text.length - search.length) * 50) / match_text.length)
      }
    
      return (matchType + matchScore) * 100
    }
    return { matchedString: match?.matchedString, score: scoreMatch(match.typeScore) }
  }
}

export interface Component {
  suggestion_id: number
  icon: string
  label: string
  score: number
  group: number
}

export const useComponentsStore = defineStore('components', () => {
  const filtering = ref<Filtering>(new Filtering('', {}))
  const suggestionDb = useSuggestionDbStore()
  const components: ComputedRef<Component[]> = computed(() => {
    const currentFiltering = filtering.value
    // function* filter<T>(iterator: Iterable<T>, pred: (elem: T) => boolean) {
    //   for (const item of iterator) {
    //     if (pred(item)) {
    //       yield item
    //     }
    //   }
    // }
    // return [
    //   ...filter(suggestionDb.entries.entries(), ([key, value]) => {
    //     currentFiltering.score(value) > 0
    //   }),
    // ]
    const components2: Component[] = Array.from(suggestionDb.entries.entries(), ([id, entry]) => {
      return {
        suggestion_id: id,
        icon: entry.icon_name,
        label:
          entry.memberOf != null ? `${entry.memberOf.split('.').pop()}.${entry.name}` : entry.name,
        score: currentFiltering.score(entry),
        group: entry.group_index,
      }
    })
    components2.sort((a, b) => a.score - b.score)
    return components2
  })

  return { components, filtering }
})
