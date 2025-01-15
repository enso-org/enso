import {
  SuggestionKind,
  type SuggestionEntry,
  type Typename,
} from '@/stores/suggestionDatabase/entry'
import type { Opt } from '@/util/data/opt'
import { Range } from '@/util/data/range'
import { qnIsTopElement, qnLastSegment, type QualifiedName } from '@/util/qualifiedName'
import escapeStringRegexp from '@/util/regexp'

export type SelfArg =
  | {
      type: 'known'
      typename: Typename
    }
  | { type: 'unknown' }

export interface Filter {
  pattern?: string
  selfArg?: SelfArg
}

export enum MatchTypeScore {
  NameWordMatchFirst = 0,
  NameWordMatch = 2000,
  NameInitialMatch = 4000,
}
const NONEXACT_MATCH_PENALTY = 50
const ALIAS_PENALTY = 1000
/** If we match by both entry name and owner name, the owner name is less important. */
const OWNER_SCORE_WEIGHT = 0.2
/** The matches on actual names should be better than matches on owner names only */
const OWNER_ONLY_MATCH_PENALTY = 6000

interface NameMatchResult {
  score: number
  ranges: Range[]
}

interface MatchedParts {
  matchedAlias?: string
  nameRanges?: Range[]
  ownerNameRanges?: Range[]
}

export interface MatchResult extends MatchedParts {
  score: number
}

class FilteringName {
  pattern: string
  wordMatchRegex: RegExp
  initialsMatchRegex?: RegExp

  constructor(pattern: string) {
    this.pattern = pattern
    // Each word in pattern should try to match a beginning of a word in the name.  Each matched
    // word is put to regex group - this is used to compute score (details in `matchedWordsScore`
    // method). See `Filtering` docs for full algorithm description.
    // The first match (`match[1]`) is the part before the first matched input.
    // The rest of the matches come in groups of three:
    // - The matched part of the word (including a leading underscore for all but the first match)
    // - The unmatched rest of the word, up to, but excluding, the next underscore
    // - The unmatched words before the next matched word, including any underscores
    this.wordMatchRegex = new RegExp(
      '(^|.*?[_ ])(' +
        escapeStringRegexp(pattern).replace(/[_ ]/g, ')([^_ ]*)(.*?)([_ ]') +
        ')([^_ ]*)(.*)',
      'i',
    )
    if (pattern.length > 1 && !/_/.test(pattern)) {
      // Similar to `wordMatchRegex`, but each letter in the pattern is considered a word,
      // and we don't skip word (initials must match consecutive words).
      // The first match (`match[1]`) is the part before the first matched letter.
      // The rest of the matches come in groups of two:
      // - The matched letter
      // - The unmatched part up to the next matched letter
      const regex = pattern
        .split('')
        .map((c) => `(${escapeStringRegexp(c)})`)
        .join('([^_ ]*?[_ ])')
      this.initialsMatchRegex = new RegExp('(^|.*?[_ ])' + regex + '(.*)', 'i')
    }
  }

  private matchedWordsScore(
    matchType: MatchTypeScore,
    matchedString: string,
    matches: RegExpExecArray,
  ): number {
    const words: string[] = []
    for (let i = 2; i < matches.length; i += 3) {
      words.push(matches[i]!, matches[i + 1]!)
    }
    const matchedWords = words.join('_')
    const nonexactMatchPenalty = this.pattern === matchedString ? 0 : NONEXACT_MATCH_PENALTY
    const nonexactWordMatchPenalty = Math.floor(
      ((matchedWords.length - this.pattern.length) * 50) / matchedWords.length,
    )
    return matchType + nonexactMatchPenalty + nonexactWordMatchPenalty
  }

  private static wordMatchRanges(wordMatch: RegExpExecArray) {
    const result: Range[] = []
    for (let i = 1, pos = 0; i < wordMatch.length; i += 1) {
      // Matches come in groups of three, and the first matched part is `match[2]`.
      if (i % 3 === 2) {
        result.push(new Range(pos, pos + wordMatch[i]!.length))
      }
      pos += wordMatch[i]!.length
    }
    return result
  }

  private static initialsMatchRanges(initialsMatch: RegExpExecArray) {
    const result: Range[] = []
    for (let i = 1, pos = 0; i < initialsMatch.length; i += 1) {
      // Matches come in groups of two, and the first matched part is `match[2]` (= 0 mod 2).
      if (i % 2 === 0) {
        result.push(new Range(pos, pos + initialsMatch[i]!.length))
      }
      pos += initialsMatch[i]!.length
    }
    return result
  }

  tryMatch(name: string): NameMatchResult | null {
    const wordsMatch = this.wordMatchRegex?.exec(name)
    if (wordsMatch?.[1]?.length === 0) {
      return {
        score: this.matchedWordsScore(MatchTypeScore.NameWordMatchFirst, name, wordsMatch),
        ranges: FilteringName.wordMatchRanges(wordsMatch),
      }
    }
    if (wordsMatch) {
      return {
        score: this.matchedWordsScore(MatchTypeScore.NameWordMatch, name, wordsMatch),
        ranges: FilteringName.wordMatchRanges(wordsMatch),
      }
    }
    if (this.initialsMatchRegex) {
      const initialsMatch = this.initialsMatchRegex.exec(name)
      if (initialsMatch) {
        return {
          score: MatchTypeScore.NameInitialMatch,
          ranges: FilteringName.initialsMatchRanges(initialsMatch),
        }
      }
    }
    return null
  }
}

class FilteringWithPattern {
  nameFilter: FilteringName
  ownerNameFilter: FilteringName
  bothFiltersMustMatch: boolean

  constructor(pattern: string) {
    const split = pattern.lastIndexOf('.')
    if (split >= 0) {
      // If there is a dot in the pattern, the segment before must match owner name,
      // and the segment after - the entry name
      this.nameFilter = new FilteringName(pattern.slice(split + 1))
      this.ownerNameFilter = new FilteringName(pattern.slice(0, split))
      this.bothFiltersMustMatch = true
    } else {
      // the pattern has to match name or the owner name
      this.nameFilter = new FilteringName(pattern)
      this.ownerNameFilter = this.nameFilter
      this.bothFiltersMustMatch = false
    }
  }

  private firstMatchingAlias(aliases: string[]) {
    for (const alias of aliases) {
      const match = this.nameFilter.tryMatch(alias)
      if (match != null) return { alias, ...match }
    }
    return null
  }

  tryMatch(name: string, aliases: string[], memberOf: QualifiedName): MatchResult | null {
    const nameMatch: (NameMatchResult & { alias?: string }) | null =
      this.nameFilter.tryMatch(name) ?? this.firstMatchingAlias(aliases)
    const ownerNameMatch = this.ownerNameFilter.tryMatch(qnLastSegment(memberOf))
    if (!nameMatch && !ownerNameMatch) return null
    if (this.bothFiltersMustMatch && (!nameMatch || !ownerNameMatch)) return null

    const result: MatchResult = { score: 0 }
    if (nameMatch) {
      result.score += nameMatch.score
      if ('alias' in nameMatch) {
        result.score += ALIAS_PENALTY
        result.matchedAlias = nameMatch.alias
      }
      result.nameRanges = nameMatch.ranges

      if (!this.bothFiltersMustMatch) return result
    }
    if (ownerNameMatch) {
      result.score +=
        ownerNameMatch.score * (nameMatch ? OWNER_SCORE_WEIGHT : 1) +
        (nameMatch ? 0 : OWNER_ONLY_MATCH_PENALTY)
      result.ownerNameRanges = ownerNameMatch.ranges
      return result
    }
    return null
  }
}

/**
 * Filtering Suggestions for Component Browser.
 *
 * A single entry is filtered in if _all_ conditions below are met:
 * - The non-private method entries are only matched.
 *
 * - If `selfArg` is specified, only entries of methods taking a value of this type as self
 *   argument are accepted (or any non-static method if the type of self argument is unknown).
 *
 * - If `pattern` is specified with dot, the part after dot must match entry name or alias, while
 *   on the left side of the dot must match type/module on which the entry is specified.
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
  selfArg?: SelfArg
  currentModule?: QualifiedName

  /** TODO: Add docs */
  constructor(filter: Filter, currentModule: Opt<QualifiedName> = undefined) {
    const { pattern, selfArg } = filter
    if (pattern) {
      this.pattern = new FilteringWithPattern(pattern)
    }
    if (selfArg != null) this.selfArg = selfArg
    if (currentModule != null) this.currentModule = currentModule
  }

  private selfTypeMatches(entry: SuggestionEntry, additionalSelfTypes: QualifiedName[]): boolean {
    if (this.selfArg == null) return entry.selfType == null
    else if (this.selfArg.type == 'known')
      return (
        entry.selfType === this.selfArg.typename ||
        additionalSelfTypes.some((t) => entry.selfType === t)
      )
    else return entry.selfType != null
  }

  /** TODO: Add docs */
  isMainView() {
    return this.pattern == null && this.selfArg == null
  }

  private mainViewFilter(entry: SuggestionEntry): MatchResult | null {
    const hasGroup = entry.groupIndex != null
    const isInTopModule = qnIsTopElement(entry.definedIn)
    if (hasGroup || isInTopModule) return { score: 0 }
    else return null
  }

  private isLocal(entry: SuggestionEntry): boolean {
    return this.currentModule != null && entry.definedIn === this.currentModule
  }

  /** TODO: Add docs */
  filter(entry: SuggestionEntry, additionalSelfTypes: QualifiedName[]): MatchResult | null {
    if (entry.isPrivate || entry.kind != SuggestionKind.Method || entry.memberOf == null)
      return null
    if (this.selfArg == null && isInternal(entry)) return null
    if (!this.selfTypeMatches(entry, additionalSelfTypes)) return null
    if (this.pattern) {
      if (entry.memberOf == null) return null
      const patternMatch = this.pattern.tryMatch(entry.name, entry.aliases, entry.memberOf)
      if (!patternMatch) return null
      if (this.isLocal(entry)) patternMatch.score *= 2
      return patternMatch
    }
    if (this.isMainView()) return this.mainViewFilter(entry)
    return { score: 0 }
  }
}

function isInternal(entry: SuggestionEntry): boolean {
  return isInternalModulePath(entry.definedIn)
}

function isInternalModulePath(path: string): boolean {
  return /Standard[.].*Internal(?:[._]|$)/.test(path)
}
