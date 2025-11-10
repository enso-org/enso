/** @file Parsing and representation of the search query. */
import { shallowEqual } from '$/utils/data/array'
import { unsafeKeyValuePair } from 'enso-common/src/utilities/data/object'

// Control characters must be handled, in order to follow the JSON spec.
// eslint-disable-next-line no-control-regex
const JSON_VALUE_REGEX = /"(?:[^\0-\x1f\\"]|\\[\\/bfnrt"]|\\u[0-9a-fA-F]{4})*"?/.source

/**
 * The regex, with `<json>` replaced with a regex subexpression matching a JSON-escaped search
 * term.
 */
function interpolateRegex(regex: RegExp) {
  return new RegExp(regex.source.replace(/<json>/g, JSON_VALUE_REGEX), regex.flags)
}

/** Keys of an {@Link AssetQuery} which correspond to tags. */
export type AssetQueryKey = Exclude<keyof AssetQuery & `${string}s`, 'withUpdates'>

/** An {@link AssetQuery}, without the query and methods. */
export type AssetQueryData = Record<AssetQueryKey, readonly string[]>

/**
 * An {@link AssetQuery}, without the query and methods, and with all the values being `string[]`s
 * instead of `string[][]`s, representing the last term rather than all terms.
 */
export type AssetQueryLastTermData = Readonly<Record<AssetQueryKey, string[]>>

/** An individual segment of a query string input to {@link AssetQuery}. */
interface AssetQueryTerm {
  readonly tag: string | null
  readonly values: readonly string[]
}

/** Parsing and representation of the search query. */
export default class AssetQuery {
  static plainValueRegex = interpolateRegex(/^(?:|[^"]\S*)$/)
  static jsonValueRegex = interpolateRegex(/^(<json>)$/)
  static termsRegex = interpolateRegex(/(?:([^\s:]*):)?((?:(?:<json>|(?:[^,\s"][^,\s]*)),?)*|)/g)
  static valuesRegex = interpolateRegex(/(?:<json>)|(?:[^,\s"][^,\s]*)/g)
  static tagNames = [
    ['keywords', null],
    ['names', 'name'],
    ['types', 'type'],
    ['extensions', 'extension'],
    ['descriptions', 'description'],
    // ['modifieds', 'modified'],
    // ['labels', 'label'],
    ['owners', 'owner'],
  ] as const satisfies readonly (readonly [keyof AssetQueryData, string | null])[]
  /** The subset of {@link AssetQuery.tagNames} that are applicable for the Local Backend. */
  static localTagNames = [
    ['keywords', null],
    ['names', 'name'],
    ['types', 'type'],
    ['extensions', 'extension'],
    // ['modifieds', 'modified'],
  ] as const satisfies readonly (readonly [keyof AssetQueryData, string | null])[]

  readonly query

  /** Create an {@link AssetQuery}. */
  constructor(
    query: string | null,
    readonly keywords: readonly string[],
    readonly names: readonly string[],
    readonly labels: readonly string[],
    readonly types: readonly string[],
    readonly extensions: readonly string[],
    readonly descriptions: readonly string[],
    readonly modifieds: readonly string[],
    readonly owners: readonly string[],
  ) {
    this.query = query ?? ''
    if (query == null) {
      this.query = this.toString()
    }
  }

  /** Return a list of {@link AssetQueryTerm}s found in the raw user input string. */
  static terms(query: string): AssetQueryTerm[] {
    const terms: AssetQueryTerm[] = []
    for (const [, tag, valuesRaw = ''] of query.trim().matchAll(this.termsRegex)) {
      // Ignore values with a tag but without a value.
      if (tag != null || valuesRaw !== '') {
        const values = valuesRaw.match(AssetQuery.valuesRegex) ?? []
        terms.push({
          tag: tag ?? null,
          values:
            valuesRaw === '' ?
              []
            : values.map((value) =>
                AssetQuery.jsonValueRegex.test(value) ?
                  String(JSON.parse(value.endsWith('"') && value.length > 1 ? value : value + '"'))
                : value,
              ),
        })
      }
    }
    return terms
  }

  /** Convert an {@link AssetQueryTerm} to a string usable in a raw user input string. */
  static termToString(term: AssetQueryTerm) {
    const tagSegment = term.tag == null ? '' : term.tag + ':'
    const valueSegment = term.values
      .map((value) => (AssetQuery.plainValueRegex.test(value) ? value : JSON.stringify(value)))
      .join(' ')
    return tagSegment + valueSegment
  }

  /** Create an {@link AssetQuery} from a raw user input string. */
  static fromString(query: string): AssetQuery {
    const terms = AssetQuery.terms(query)
    const keywords: string[] = []
    const names: string[] = []
    const labels: string[] = []
    const types: string[] = []
    const extensions: string[] = []
    const descriptions: string[] = []
    const modifieds: string[] = []
    const owners: string[] = []
    const tagNameToSet: Readonly<Record<string, string[]>> = {
      // This is a dictionary, not an object.
      /* eslint-disable @typescript-eslint/naming-convention */
      '': keywords,
      name: names,
      label: labels,
      type: types,
      extension: extensions,
      ext: extensions,
      description: descriptions,
      desc: descriptions,
      modified: modifieds,
      owner: owners,
      /* eslint-enable @typescript-eslint/naming-convention */
    }
    for (const term of terms) {
      if (term.tag == null) {
        keywords.push(...term.values)
      } else {
        tagNameToSet[term.tag]?.push(...term.values)
      }
    }
    return new AssetQuery(
      query,
      keywords,
      names,
      labels,
      types,
      extensions,
      descriptions,
      modifieds,
      owners,
    )
  }

  /** Return a new array of terms, after applying the given updates. */
  static updatedTerms(
    original: readonly string[],
    toAdd: readonly string[] | null,
    toRemove: readonly string[] | null,
  ) {
    toAdd = toAdd?.length === 0 ? null : toAdd
    toRemove = toRemove?.length === 0 ? null : toRemove
    if (toAdd == null && (toRemove == null || original.length === 0)) {
      return null
    } else {
      let terms =
        original.some((term) => term === '') ? original.filter((term) => term !== '') : original
      let changed = terms === original
      if (toAdd != null) {
        const termsAfterAdditions = [
          ...terms,
          ...toAdd.filter((otherTerm) =>
            terms.every((term) => !shallowEqual([...term].sort(), [...otherTerm].sort())),
          ),
        ]
        if (termsAfterAdditions.length !== terms.length) {
          terms = termsAfterAdditions
          changed = true
        }
      }
      if (toRemove != null) {
        const termsAfterRemovals = terms.filter((term) =>
          toRemove.every((otherTerm) => !shallowEqual([...term].sort(), [...otherTerm].sort())),
        )
        if (termsAfterRemovals.length !== terms.length) {
          terms = termsAfterRemovals
          changed = true
        }
      }
      return !changed ? null : terms
    }
  }

  /** Create an identical copy of this query. Useful to force a React refresh. */
  clone() {
    return new AssetQuery(
      this.query,
      this.keywords,
      this.names,
      this.labels,
      this.types,
      this.extensions,
      this.descriptions,
      this.modifieds,
      this.owners,
    )
  }

  /**
   * Return a new {@link AssetQuery} with the specified keys overwritten,
   * or itself if there are no keys to overwrite.
   */
  withUpdates(updates: Partial<AssetQueryData>) {
    if (Object.keys(updates).length === 0) {
      return this
    } else {
      return new AssetQuery(
        null,
        updates.keywords ?? this.keywords,
        updates.names ?? this.names,
        updates.labels ?? this.labels,
        updates.types ?? this.types,
        updates.extensions ?? this.extensions,
        updates.descriptions ?? this.descriptions,
        updates.modifieds ?? this.modifieds,
        updates.owners ?? this.owners,
      )
    }
  }

  /** Return a new {@link AssetQuery} with the specified terms added. */
  add(key: AssetQueryKey, value: readonly string[]): AssetQuery {
    const update = AssetQuery.updatedTerms(this[key], value, null)
    if (!update) return this
    return this.withUpdates(unsafeKeyValuePair(key, update))
  }

  /** Return a new {@link AssetQuery} with the specified terms deleted. */
  delete(key: AssetQueryKey, value: readonly string[]): AssetQuery {
    const update = AssetQuery.updatedTerms(this[key], null, value)
    if (!update) return this
    return this.withUpdates(unsafeKeyValuePair(key, update))
  }

  /** Try to cycle the tag between present, and not present. */
  withToggled(positiveTag: AssetQueryKey, value: string) {
    // This aliasing is INTENTIONAL because the variable is (potentially) reassigned.
    // eslint-disable-next-line @typescript-eslint/no-this-alias
    let newQuery: AssetQuery = this
    newQuery = newQuery.delete(positiveTag, [value])
    if (newQuery === this) {
      newQuery = newQuery.add(positiveTag, [value])
    }
    return newQuery
  }

  /** Returns a string representation usable in the search bar. */
  toString() {
    const segments: string[] = []
    for (const [key, tag] of AssetQuery.tagNames) {
      const values = this[key]
      if (values.length === 0) {
        continue
      }
      segments.push(AssetQuery.termToString({ tag, values }))
    }
    return segments.join(' ')
  }
}
