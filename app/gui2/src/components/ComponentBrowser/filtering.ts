import { SuggestionKind, type SuggestionEntry } from '@/stores/suggestionDatabase/entry'
import { qnParent, type QualifiedName } from '@/util/qualifiedName'

export interface Filter {
  pattern?: string
  selfType?: QualifiedName
  qualifiedNamePattern?: QualifiedName
  showUnstable?: boolean
  showLocal?: boolean
}

export enum MatchTypeScore {
  // Words are matched, including the first word.
  NameWordMatchFirst = 0,
  // Words are matched, including the first word.
  AliasWordMatchFirst = 1000,
  NameWordMatch = 2000,
  AliasWordMatch = 3000,
  NameInitialMatch = 4000,
  AliasInitialMatch = 5000,
}

export type MatchResult = {
  matchedAlias?: string
  score: number
} | null

class FilteringWithPattern {
  pattern: string
  wordMatchRegex: RegExp
  initialsMatchRegex?: RegExp

  constructor(pattern: string) {
    this.pattern = pattern
    // Todo[ao]: Document somewhere this regexp.
    this.wordMatchRegex = new RegExp(
      '(?:^|_)(' + pattern.replaceAll('_', '[^_]*).*?_(') + '[^_]*).*',
      'i',
    )
    if (pattern.length > 1) {
      this.initialsMatchRegex = new RegExp('(^|_)' + pattern.split('').join('.*_'), 'i')
    }
  }

  private matchedWordsScore(
    matchType: MatchTypeScore,
    matchedString: string,
    words: RegExpExecArray,
  ): number {
    words.shift()
    const matchedWords = words.join('_')
    const nonexactMatchPenalty = this.pattern === matchedString ? 0 : 50
    const nonexactWordMatchPenalty = Math.floor(
      ((matchedWords.length - this.pattern.length) * 50) / matchedWords.length,
    )
    return matchType + nonexactMatchPenalty + nonexactWordMatchPenalty
  }

  private firstMatchingAlias(entry: SuggestionEntry) {
    for (const alias of entry.aliases) {
      const match = this.wordMatchRegex.exec(alias)
      if (match != null) return { alias, match }
    }
    return null
  }

  tryMatch(entry: SuggestionEntry): MatchResult {
    const nameWordsMatch = this.wordMatchRegex?.exec(entry.name)
    if (nameWordsMatch?.index === 0) {
      return {
        score: this.matchedWordsScore(
          MatchTypeScore.NameWordMatchFirst,
          entry.name,
          nameWordsMatch,
        ),
      }
    }
    const matchedAlias = this.firstMatchingAlias(entry)
    if (matchedAlias?.match.index === 0) {
      return {
        matchedAlias: matchedAlias.alias,
        score: this.matchedWordsScore(
          MatchTypeScore.AliasWordMatchFirst,
          matchedAlias.alias,
          matchedAlias.match,
        ),
      }
    }
    if (nameWordsMatch) {
      return {
        score: this.matchedWordsScore(MatchTypeScore.NameWordMatch, entry.name, nameWordsMatch),
      }
    }
    if (matchedAlias) {
      return {
        matchedAlias: matchedAlias.alias,
        score: this.matchedWordsScore(
          MatchTypeScore.AliasWordMatch,
          matchedAlias.alias,
          matchedAlias.match,
        ),
      }
    }
    if (this.initialsMatchRegex) {
      if (this.initialsMatchRegex.test(entry.name)) {
        return { score: MatchTypeScore.NameInitialMatch }
      }
      const matchedAliasInitials = entry.aliases.find(
        (alias) => this.initialsMatchRegex?.test(alias),
      )
      if (matchedAliasInitials) {
        return { matchedAlias: matchedAliasInitials, score: MatchTypeScore.AliasInitialMatch }
      }
    }

    return null
  }
}

class FilteringQualifiedName {
  pattern: string
  memberRegex: RegExp
  memberOfAnyDescendantRegex: RegExp

  constructor(pattern: string) {
    this.pattern = pattern
    const segmentsMatch = '(^|\\.)' + pattern.replaceAll('.', '[^\\.]*\\.')
    this.memberRegex = new RegExp(segmentsMatch + '[^\\.]*$', 'i')
    this.memberOfAnyDescendantRegex = new RegExp(segmentsMatch, 'i')
  }

  matches(entry: SuggestionEntry, alsoFilteringByPattern: boolean): boolean {
    const entryOwner =
      entry.kind == SuggestionKind.Module ? qnParent(entry.definedIn) : entry.definedIn
    const regex = alsoFilteringByPattern ? this.memberOfAnyDescendantRegex : this.memberRegex
    return regex.test(entryOwner) || (entry.memberOf != null && regex.test(entry.memberOf))
  }
}

export class Filtering {
  pattern?: FilteringWithPattern
  selfType?: QualifiedName
  qualifiedName?: FilteringQualifiedName
  showUnstable: boolean = false
  showLocal: boolean = false

  constructor(filter: Filter) {
    const { pattern, selfType, qualifiedNamePattern, showUnstable, showLocal } = filter
    if (pattern != null && pattern !== '') {
      this.pattern = new FilteringWithPattern(pattern)
    }
    this.selfType = selfType
    if (qualifiedNamePattern != null && qualifiedNamePattern !== '') {
      this.qualifiedName = new FilteringQualifiedName(qualifiedNamePattern)
    }
    this.showUnstable = showUnstable ?? false
    this.showLocal = showLocal ?? false
  }

  private selfTypeMatches(entry: SuggestionEntry): boolean {
    if (this.selfType == null) {
      return entry.selfType == null
    } else {
      return entry.selfType === this.selfType
    }
  }

  private qualifiedNameMatches(entry: SuggestionEntry): boolean {
    if (this.qualifiedName == null) return true
    return this.qualifiedName.matches(entry, this.pattern != null)
  }

  isMainView() {
    return (
      this.pattern == null && this.selfType == null && this.qualifiedName == null && !this.showLocal
    )
  }

  private mainViewFilter(entry: SuggestionEntry) {
    const hasGroup = entry.groupIndex != null
    const isModule = entry.kind === SuggestionKind.Module
    const isTopElement = (entry.definedIn.match(/\./g)?.length ?? 0) <= 2
    if (hasGroup || (isModule && isTopElement)) {
      return { score: 0 }
    } else {
      return null
    }
  }

  filter(entry: SuggestionEntry): MatchResult {
    if (!this.selfTypeMatches(entry)) return null
    else if (!this.qualifiedNameMatches(entry)) return null
    else if (!this.showUnstable && entry.isUnstable) return null
    else if (this.pattern) return this.pattern.tryMatch(entry)
    else if (this.isMainView()) return this.mainViewFilter(entry)
    else return { score: 0 }
  }
}
