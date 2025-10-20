import { TypeInfo } from '$/providers/openedProjects/project/computedValueRegistry'
import { SuggestionDb } from '$/providers/openedProjects/suggestionDatabase'
import {
  SuggestionKind,
  type SuggestionEntry,
} from '$/providers/openedProjects/suggestionDatabase/entry'
import { ANY_TYPE } from '@/util/ensoTypes'
import type { ProjectPath } from '@/util/projectPath'
import { qnLastSegment } from '@/util/qualifiedName'
import escapeStringRegexp from '@/util/regexp'
import { Range } from 'ydoc-shared/util/data/range'

export type SelfArg =
  | {
      type: 'known'
      typeInfo: TypeInfo
      ancestors: ProjectPath[]
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
/** Penalty added when selfType is specified, and we match entry from another type */
const DIFFERENT_TYPE_PENALTY = 1

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
  /** Score of the match. Lower is better. */
  score: number
  /**
   * Populated only if matched entry is provided by ‘additional’ type of the self argument, like methods of `Column` type for single-column table.
   * It is used for type casting suggestions.
   */
  fromType: ProjectPath | undefined
}

function exactMatch(): MatchResult {
  return { score: 0, fromType: undefined }
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
        result.push(Range.fromStartAndLength(pos, wordMatch[i]!.length))
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
        result.push(Range.fromStartAndLength(pos, initialsMatch[i]!.length))
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
  nameFilter: FilteringName | null
  ownerNameFilter: FilteringName
  bothFiltersMustMatch: boolean

  constructor(pattern: string) {
    const isTypeFiltering = pattern.startsWith(':')
    if (isTypeFiltering) pattern = pattern.slice(1)
    const split = pattern.lastIndexOf('.')
    if (split >= 0) {
      // If there is a dot in the pattern, the segment before must match owner name,
      // and the segment after - the entry name
      this.nameFilter = new FilteringName(pattern.slice(split + 1))
      this.ownerNameFilter = new FilteringName(pattern.slice(0, split))
      this.bothFiltersMustMatch = true
    } else if (isTypeFiltering) {
      // the pattern has to match the owner name
      this.nameFilter = null
      this.ownerNameFilter = new FilteringName(pattern)
      this.bothFiltersMustMatch = false
    } else {
      // the pattern has to match name or the owner name
      this.nameFilter = new FilteringName(pattern)
      this.ownerNameFilter = this.nameFilter
      this.bothFiltersMustMatch = false
    }
  }

  private firstMatchingAlias(aliases: string[]) {
    for (const alias of aliases) {
      const match = this.nameFilter?.tryMatch(alias)
      if (match != null) return { alias, ...match }
    }
    return null
  }

  tryMatch(
    name: string,
    aliases: string[],
    memberOf: ProjectPath,
    additionalSelfTypes: ProjectPath[],
  ): MatchResult | null {
    const nameMatch: (NameMatchResult & { alias?: string }) | null =
      this.nameFilter?.tryMatch(name) ?? this.firstMatchingAlias(aliases)
    const ownerNameMatch = this.ownerNameFilter.tryMatch(
      memberOf.path ? qnLastSegment(memberOf.path) : 'Main',
    )
    if (!nameMatch && !ownerNameMatch) return null
    if (this.bothFiltersMustMatch && (!nameMatch || !ownerNameMatch)) return null

    const fromType = additionalSelfTypes.find((t) => t.equals(memberOf)) ? memberOf : undefined
    const result: MatchResult = { score: 0, fromType }
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
 *   there must exist a subsequence of words in name/alias (words are separated by `_`), so each
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
  pattern: FilteringWithPattern | undefined
  selfArg: SelfArg | undefined

  /** TODO: Add docs */
  constructor(
    filter: Filter,
    public currentModule: ProjectPath | undefined = undefined,
  ) {
    const { pattern, selfArg } = filter
    this.pattern = pattern ? new FilteringWithPattern(pattern) : undefined
    this.selfArg = selfArg
  }

  private selfTypeMatches(entry: SuggestionEntry): MatchResult | null {
    if (this.selfArg == null)
      return entry.kind !== SuggestionKind.Method || entry.selfType == null ? exactMatch() : null
    if (entry.kind !== SuggestionKind.Method || entry.selfType == null) return null
    if (this.selfArg.type !== 'known') return exactMatch()
    const entrySelfType = entry.selfType
    const visibleTypes = this.selfArg.typeInfo.visibleTypes
    const visibleTypeMatch = visibleTypes?.find((ty) => entrySelfType.equals(ty))
    if (visibleTypeMatch != null) return exactMatch()
    const hiddenTypeMatch = this.selfArg.typeInfo?.hiddenTypes.find((t) => entrySelfType.equals(t))
    const matchedAncestor = this.selfArg.ancestors.find((t) => entrySelfType.equals(t))
    if (entrySelfType.equals(ANY_TYPE) || hiddenTypeMatch != null || matchedAncestor != null)
      // Matched ancestor are not added to `fromType`, because type casting is not needed.
      return { score: DIFFERENT_TYPE_PENALTY, fromType: hiddenTypeMatch }
    return null
  }

  /** TODO: Add docs */
  isMainView() {
    return this.pattern == null && this.selfArg == null
  }

  private mainViewFilter(entry: SuggestionEntry): MatchResult | null {
    const hasGroup = entry.groupIndex != null
    const isInTopModule = entry.definedIn.isTopElement()
    if (hasGroup || isInTopModule) return exactMatch()
    else return null
  }

  private isLocal(entry: SuggestionEntry): boolean {
    return this.currentModule != null && entry.definedIn.equals(this.currentModule)
  }

  /**
   * Check if given entry matches the filtering criteria.
   *
   * - If {@link selfArg} is available, it is used to filter out methods that do not match the self type.
   * - If {@link pattern} is available, it is used to narrow down the list further.
   * - When {@link selfArg} is not available, {@link mainViewFilter} is used to only display
   * entries with a group defined or in the top module.
   */
  filter(entry: SuggestionEntry, db: SuggestionDb): MatchResult | null {
    if (entry.isPrivate || entry.kind != SuggestionKind.Method) return null
    if (this.selfArg == null && isInternal(entry)) return null
    let result = this.selfTypeMatches(entry)
    if (result == null) return null
    if (this.pattern) {
      const additionalSelfTypes =
        this.selfArg?.type === 'known' ? this.selfArg.typeInfo.hiddenTypes : []
      const patternMatch = this.pattern.tryMatch(
        entry.name,
        entry.aliasesAndMacros,
        entry.memberOf,
        additionalSelfTypes,
      )
      if (!patternMatch) return null
      if (this.isLocal(entry)) patternMatch.score *= 2
      patternMatch.score += result.score
      result = patternMatch
    } else if (this.isMainView()) {
      result = this.mainViewFilter(entry)
      if (result == null) return null
    }

    // Defer the expensive constructor privacy check until all other filters pass.
    if (entry.kind === SuggestionKind.Method) {
      const constructors = db.lookupConstructorField(entry.memberOf, entry.name)
      const allPrivate =
        constructors.size > 0 && [...constructors].every((id) => db.get(id)?.isPrivate)
      if (allPrivate) return null
    }

    return result
  }
}

function isInternal(entry: SuggestionEntry): boolean {
  return isInternalModulePath(entry.definedIn)
}

function isInternalModulePath({ project, path }: ProjectPath): boolean {
  return !!project && project.startsWith('Standard.') && !!path && /Internal(?:[._]|$)/.test(path)
}
