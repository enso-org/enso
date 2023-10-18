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

export interface MatchRange {
  start: number
  end: number
  isMatch: boolean
}

export interface PartialMatchResult {
  matchedAlias?: string
  score: number
}

export interface MatchResult extends PartialMatchResult {
  matchedRanges?: MatchRange[]
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

  tryMatch(entry: SuggestionEntry): Opt<PartialMatchResult> {
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
  selfType?: QualifiedName | undefined
  qualifiedName?: FilteringQualifiedName
  fullPattern: string | undefined
  /** The first and last match are the parts of the string that are outside of the match.
   * The middle matches come in groups of three, and contain respectively:
   * - the unmatched prefix (must end with a `_`)
   *   (an empty string if the entire qualified name segment was matched)
   * - the matched text
   * - the unmatched suffix (an empty string if the entire qualified name segment was matched)
   * - the separator (`.` or `_`, or the empty string if this is the last segment) */
  extractMatchesRegex: RegExp | undefined
  showUnstable: boolean = false
  showLocal: boolean = false

  constructor(filter: Filter) {
    const { pattern, selfType, qualifiedNamePattern, showUnstable, showLocal } = filter
    if (pattern) {
      this.pattern = new FilteringWithPattern(pattern)
    }
    this.selfType = selfType
    if (qualifiedNamePattern) {
      this.qualifiedName = new FilteringQualifiedName(qualifiedNamePattern)
    }
    if (qualifiedNamePattern) {
      this.fullPattern = pattern ? `${qualifiedNamePattern}.${pattern}` : qualifiedNamePattern
    } else {
      this.fullPattern = pattern
    }
    if (this.fullPattern) {
      let prefix = ''
      let suffix = ''
      for (const [, text, separator] of this.fullPattern.matchAll(/(.+?)([._]|$)/g)) {
        const segment =
          separator === '_'
            ? `()(${text})([^_.]*)(_)`
            : `([^.]*_)?(${text})([^.]*)(${separator === '.' ? '\\.' : ''})`
        prefix = '(?:' + prefix
        suffix += segment + ')?'
      }
      this.extractMatchesRegex = new RegExp('^(.*?)' + prefix + suffix + '(.*)$', 'i')
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
    if (!hasGroup && (!isModule || !isTopElement)) return
    else return { score: 0 }
  }

  private partialFilter(entry: SuggestionEntry): Opt<PartialMatchResult> {
    if (entry.isPrivate) return
    else if (!this.selfTypeMatches(entry)) return
    else if (!this.qualifiedNameMatches(entry)) return
    else if (!this.showUnstable && entry.isUnstable) return
    else if (this.pattern) return this.pattern.tryMatch(entry)
    else if (this.isMainView()) return this.mainViewFilter(entry)
    else return { score: 0 }
  }

  private static addMatchRange(ranges: MatchRange[], length: number | undefined, isMatch: boolean) {
    if (!length) return
    const lastRange = ranges[ranges.length - 1]
    if (lastRange == null) {
      ranges.push({ start: 0, end: length, isMatch })
      return
    }
    if (lastRange.isMatch === isMatch) {
      lastRange.end += length
    } else {
      ranges.push({ start: lastRange.end, end: lastRange.end + length, isMatch })
    }
  }

  filter(entry: SuggestionEntry): Opt<MatchResult> {
    const partialResult = this.partialFilter(entry)
    if (partialResult == null) return
    if (this.extractMatchesRegex == null) return partialResult
    const match = (partialResult.matchedAlias ?? entry.name).match(this.extractMatchesRegex)
    if (match == null) return partialResult
    const ranges: MatchRange[] = []
    Filtering.addMatchRange(ranges, match[1]?.length, false)
    const end = match.length - 4
    for (let i = 2; i < end; i += 4) {
      // Unmatched prefix
      Filtering.addMatchRange(ranges, match[i]?.length, false)
      // Match
      Filtering.addMatchRange(ranges, match[i + 1]?.length, true)
      // Unmatched suffix
      Filtering.addMatchRange(ranges, match[i + 2]?.length, false)
      // Separator
      Filtering.addMatchRange(ranges, match[i + 3]?.length, true)
    }
    Filtering.addMatchRange(ranges, match[match.length - 1]?.length, false)
    console.log(match, ranges, ranges[ranges.length - 1]?.end)
    return { ...partialResult, matchedRanges: ranges }
  }
}
