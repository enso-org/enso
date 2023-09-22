import { SuggestionKind, type SuggestionEntry } from '@/stores/suggestionDatabase/entry'
import type { Opt } from '@/util/opt'
import { qnParent, type QualifiedName } from '@/util/qualifiedName'

export interface Filter {
  pattern?: string
  selfType?: QualifiedName
  qualifiedNamePattern?: string
  showUnstable?: boolean
  showLocal?: boolean
}

export enum MatchTypeScore {
  NameWordMatchFirst = 0,
  AliasWordMatchFirst = 1000,
  NameWordMatch = 2000,
  AliasWordMatch = 3000,
  NameInitialMatch = 4000,
  AliasInitialMatch = 5000,
}

export type MatchResult = {
  matchedAlias?: string
  score: number
}

class FilteringWithPattern {
  pattern: string
  wordMatchRegex: RegExp
  initialsMatchRegex?: RegExp

  constructor(pattern: string) {
    this.pattern = pattern
    // Each word in pattern should try to match a beginning of a word in the name.  Each matched
    // word is put to regex group - this is used to compute score (details in matchedWordsScore
    // method). See `Filtering` docs for full algorithm description.
    this.wordMatchRegex = new RegExp(
      '(?:^|_)(' + pattern.replace(/_/g, '[^_]*).*?_(') + '[^_]*).*',
      'i',
    )
    if (pattern.length > 1 && pattern.indexOf('_') < 0) {
      // Similar to wordMatchRegex, but each letter in pattern is considered a word (and we don't
      // specify any groups).
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

  tryMatch(entry: SuggestionEntry): Opt<MatchResult> {
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
    // Starting at some segment, each segment should start with the respective
    // pattern's segment. See `Filtering` docs for full algorithm description.
    const segmentsMatch = '(^|\\.)' + pattern.replace(/\./g, '[^\\.]*\\.')
    // The direct members must have no more segments in their path.
    this.memberRegex = new RegExp(segmentsMatch + '[^\\.]*$', 'i')
    this.memberOfAnyDescendantRegex = new RegExp(segmentsMatch, 'i')
  }

  matches(entry: SuggestionEntry, alsoFilteringByPattern: boolean): boolean {
    const entryOwner =
      entry.kind == SuggestionKind.Module ? qnParent(entry.definedIn) : entry.definedIn
    const regex = alsoFilteringByPattern ? this.memberOfAnyDescendantRegex : this.memberRegex
    return (
      (entryOwner != null && regex.test(entryOwner)) ||
      (entry.memberOf != null && regex.test(entry.memberOf))
    )
  }
}

/**
 * Filtering Suggestions for Component Browser.
 *
 * A single entry is filtered in if _all_ conditions below are met:
 *
 * - The private entries never matches.
 *
 * - If `selfType` is specified, only entries of methods taking a value of this type as self
 *   argument are accepted. Static methods, and methods of other types are filtered out.
 *
 * - If `qualifiedNamePattern` is specified, only entries being a content of a module or type
 *   matching the pattern are accepted. If `pattern` is also specified (see below), the content
 *   of any descendant of the module is included too. The module/type qualified name matches
 *   a pattern with `n` segments, if its last `n` segments starts with the respective pattern's
 *   segments. For example 'local.Project.Main' is matched by 'Project.Main' or 'l.Proj.M'
 *   patterns.
 *
 * - Without `showUnstable` flag, unstable entries will be filtered out.
 *
 * - 'showLocal' flag is not implemented yet.
 *
 * - Finally, if `pattern` is specified, the entry name or any alias must match the pattern:
 *   there must exists a subsequence of words in name/alias (words are separated by `_`), so each
 *   word:
 *   - starts with respective word in the pattern,
 *   - or starts with respective _letter_ in the pattern (initials match).
 *   For example `foo_bar_baz` name is matched by patterns `foo`, `bar`, `f_b` or `ba_ba`,
 *   `fbb` or `bb`.
 *
 * For more examples, see various matching/not matching test cases in `__tests__/filtering.test.ts`
 *
 * When matched, a matching score is computed; the lower the score, the better is match. The exact
 * matches are the best, matching first word is preferred over matching other words, and matching
 * name is preferred before alias. See `FilteringWithPattern.tryMatch` implementation for details.
 */
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

  filter(entry: SuggestionEntry): Opt<MatchResult> {
    if (entry.isPrivate) return null
    else if (!this.selfTypeMatches(entry)) return null
    else if (!this.qualifiedNameMatches(entry)) return null
    else if (!this.showUnstable && entry.isUnstable) return null
    else if (this.pattern) return this.pattern.tryMatch(entry)
    else if (this.isMainView()) return this.mainViewFilter(entry)
    else return { score: 0 }
  }
}
