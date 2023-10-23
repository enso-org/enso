import { SuggestionKind, type SuggestionEntry } from '@/stores/suggestionDatabase/entry'
import type { Opt } from '@/util/opt'
import { qnIsTopElement, qnParent, type QualifiedName } from '@/util/qualifiedName'
import type { Range } from '@/util/range'

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

interface MatchedParts {
  matchedAlias?: string
  nameRanges?: Range[]
  definedInRanges?: Range[]
  memberOfRanges?: Range[]
}

export interface MatchResult extends MatchedParts {
  score: number
}

class FilteringWithPattern {
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
      '(^|.*?_)(' + pattern.replace(/_/g, ')([^_]*)(.*?)(_') + ')([^_]*)(.*)',
      'i',
    )
    if (pattern.length > 1 && !/_/.test(pattern)) {
      // Similar to `wordMatchRegex`, but each letter in the pattern is considered a word.
      // The first match (`match[1]`) is the part before the first matched letter.
      // The rest of the matches come in groups of two:
      // - The matched letter
      // - The unmatched part up to the next matched letter
      const regex = pattern
        .split('')
        .map((c) => `(${c})`)
        .join('(.*?_)')
      this.initialsMatchRegex = new RegExp('(^|.*?_)' + regex + '(.*)', 'i')
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

  private static wordMatchRanges(wordMatch: RegExpExecArray) {
    const result: Range[] = []
    for (let i = 1, pos = 0; i < wordMatch.length; i += 1) {
      // Matches come in groups of three, and the first matched part is `match[2]`.
      if (i % 3 === 2) {
        result.push({ start: pos, end: pos + wordMatch[i]!.length })
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
        result.push({ start: pos, end: pos + initialsMatch[i]!.length })
      }
      pos += initialsMatch[i]!.length
    }
    return result
  }

  tryMatch(entry: SuggestionEntry): MatchResult | null {
    const nameWordsMatch = this.wordMatchRegex?.exec(entry.name)
    if (nameWordsMatch?.[1]?.length === 0) {
      return {
        score: this.matchedWordsScore(
          MatchTypeScore.NameWordMatchFirst,
          entry.name,
          nameWordsMatch,
        ),
        nameRanges: FilteringWithPattern.wordMatchRanges(nameWordsMatch),
      }
    }
    const matchedAlias = this.firstMatchingAlias(entry)
    if (matchedAlias?.match?.[1]?.length === 0) {
      return {
        matchedAlias: matchedAlias.alias,
        score: this.matchedWordsScore(
          MatchTypeScore.AliasWordMatchFirst,
          matchedAlias.alias,
          matchedAlias.match,
        ),
        nameRanges: FilteringWithPattern.wordMatchRanges(matchedAlias.match),
      }
    }
    if (nameWordsMatch) {
      return {
        score: this.matchedWordsScore(MatchTypeScore.NameWordMatch, entry.name, nameWordsMatch),
        nameRanges: FilteringWithPattern.wordMatchRanges(nameWordsMatch),
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
        nameRanges: FilteringWithPattern.wordMatchRanges(matchedAlias.match),
      }
    }
    if (this.initialsMatchRegex) {
      const initialsMatch = this.initialsMatchRegex.exec(entry.name)
      if (initialsMatch) {
        return {
          score: MatchTypeScore.NameInitialMatch,
          nameRanges: FilteringWithPattern.initialsMatchRanges(initialsMatch),
        }
      }
      for (const alias of entry.aliases) {
        const initialsMatch = this.initialsMatchRegex.exec(alias)
        if (initialsMatch) {
          return {
            matchedAlias: alias,
            score: MatchTypeScore.AliasInitialMatch,
            nameRanges: FilteringWithPattern.initialsMatchRanges(initialsMatch),
          }
        }
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
    // For both regexes below:
    // The first match (`match[1]`) is the part before the first matched input.
    // The rest of the matches come in pairs:
    // - The matched segment
    // - The unmatched part before the next matched segment
    const segmentsMatch = '(^|.*?[.])(' + pattern.replace(/[.]/g, ')([^.]*)([.]') + ')'
    // The direct members must have no more segments in their path.
    this.memberRegex = new RegExp(segmentsMatch + '([^.]*$)', 'i')
    this.memberOfAnyDescendantRegex = new RegExp(segmentsMatch + '(.*)', 'i')
  }

  private static matchRanges(match: RegExpExecArray) {
    const result: Range[] = []
    for (let i = 1, pos = 0; i < match.length; i += 1) {
      // Matches come in groups of two, and the first matched part is `match[2]` (= 0 mod 2).
      if (i % 2 === 0) {
        result.push({ start: pos, end: pos + match[i]!.length })
      }
      pos += match[i]!.length
    }
    return result
  }

  matches(entry: SuggestionEntry, alsoFilteringByPattern: boolean): MatchedParts | null {
    const entryOwner =
      entry.kind == SuggestionKind.Module ? qnParent(entry.definedIn) : entry.definedIn
    const regex = alsoFilteringByPattern ? this.memberOfAnyDescendantRegex : this.memberRegex
    const ownerMatch = entryOwner && regex.exec(entryOwner)
    if (ownerMatch) return { definedInRanges: FilteringQualifiedName.matchRanges(ownerMatch) }
    const memberOfMatch = entry.memberOf && regex.exec(entry.memberOf)
    if (memberOfMatch) return { definedInRanges: FilteringQualifiedName.matchRanges(memberOfMatch) }
    return null
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
 * - If 'showLocal' flag is set, only entries defined in currentModule (passed as constructor
 *   argument) are accepted.
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
  currentModule?: QualifiedName

  constructor(filter: Filter, currentModule: Opt<QualifiedName> = undefined) {
    const { pattern, selfType, qualifiedNamePattern, showUnstable, showLocal } = filter
    if (pattern) {
      this.pattern = new FilteringWithPattern(pattern)
    }
    this.selfType = selfType
    if (qualifiedNamePattern) {
      this.qualifiedName = new FilteringQualifiedName(qualifiedNamePattern)
      this.fullPattern = pattern ? `${qualifiedNamePattern}.${pattern}` : qualifiedNamePattern
    } else if (pattern) this.fullPattern = pattern
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
    if (currentModule != null) this.currentModule = currentModule
  }

  private selfTypeMatches(entry: SuggestionEntry): boolean {
    if (this.selfType == null) {
      return entry.selfType == null
    } else {
      return entry.selfType === this.selfType
    }
  }

  private qualifiedNameMatches(entry: SuggestionEntry): MatchedParts | null {
    if (this.qualifiedName == null) return {}
    else return this.qualifiedName.matches(entry, this.pattern != null)
  }

  isMainView() {
    return (
      this.pattern == null && this.selfType == null && this.qualifiedName == null && !this.showLocal
    )
  }

  private mainViewFilter(entry: SuggestionEntry): MatchResult | null {
    const hasGroup = entry.groupIndex != null
    const isModule = entry.kind === SuggestionKind.Module
    const isTopElement = qnIsTopElement(entry.definedIn)
    if (!hasGroup && (!isModule || !isTopElement)) return null
    else return { score: 0 }
  }

  filter(entry: SuggestionEntry): MatchResult | null {
    let qualifiedNameMatch: Opt<MatchedParts>
    if (entry.isPrivate) return null
    else if (!this.selfTypeMatches(entry)) return null
    else if (!(qualifiedNameMatch = this.qualifiedNameMatches(entry))) return null
    else if (!this.showUnstable && entry.isUnstable) return null
    else if (
      this.showLocal &&
      (this.currentModule == null || entry.definedIn !== this.currentModule)
    )
      return null
    else if (this.pattern) {
      const patternMatch = this.pattern.tryMatch(entry)
      if (!patternMatch || !qualifiedNameMatch) return patternMatch
      else return { ...qualifiedNameMatch, ...patternMatch }
    } else if (this.isMainView()) return this.mainViewFilter(entry)
    else return { score: 0 }
  }
}
