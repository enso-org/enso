/** @file Parsing and representation of the search query. */
import * as array from './array'

// =====================
// === Regex Helpers ===
// =====================

// Control characters must be handled, in order to follow the JSON spec.
// eslint-disable-next-line no-control-regex
const JSON_VALUE_REGEX = /"(?:[^\0-\x1f\\"]|\\[\\/bfnrt"]|\\u[0-9a-fA-F]{4})*"?/.source

/** The regex, with `<json>` replaced with a regex subexpression matching a JSON-escaped search
 * term. */
function interpolateRegex(regex: RegExp) {
    return new RegExp(regex.source.replace(/<json>/g, JSON_VALUE_REGEX), regex.flags)
}

// ==================
// === AssetQuery ===
// ==================

/** Removes the `readonly` modifier from all keys in the given type. */
type Mutable<T> = { -readonly [K in keyof T]: T[K] }

/** An {@link AssetQuery}, without the query and methods. */
interface AssetQueryData
    extends Mutable<Omit<AssetQuery, 'add' | 'delete' | 'query' | 'toString'>> {}

/** An {@link AssetQuery}, without the query and methods, and with all the values being `string[]`s
 * instead of `string[][]`s, representing the last term rather than all terms. */
interface AssetQueryLastTermData
    extends Record<Exclude<keyof AssetQuery, 'add' | 'delete' | 'query' | 'toString'>, string[]> {}

/** An individual segment of a query string input to {@link AssetQuery}. */
interface AssetQueryTerm {
    tag: string | null
    values: string[]
}

/** Parsing and representation of the search query. */
export class AssetQuery {
    static plainValueRegex = interpolateRegex(/^(?:|[^"]\S*)$/)
    static jsonValueRegex = interpolateRegex(/^(<json>)$/)
    static termsRegex = interpolateRegex(/(?:([^\s:]*):)?((?:(?:<json>|(?:[^,\s"][^,\s]*)),?)*|)/g)
    static valuesRegex = interpolateRegex(/(?:<json>)|(?:[^,\s"][^,\s]*)/g)
    // `key` MUST be a string literal type.
    // eslint-disable-next-line no-restricted-syntax
    static dataKeys = [
        'keywords',
        'negativeKeywords',
        'labels',
        'negativeLabels',
        'owners',
        'negativeOwners',
    ] as const
    // `key` MUST be a string literal type.
    // eslint-disable-next-line no-restricted-syntax
    static tagNames = [
        ['keywords', null],
        ['negativeKeywords', '-name'],
        ['labels', 'label'],
        ['negativeLabels', '-label'],
        ['owners', 'owner'],
        ['negativeOwners', '-owner'],
    ] as const

    query

    /** Create an {@link AssetQuery}. */
    constructor(
        query: string | null,
        readonly keywords: string[][],
        readonly negativeKeywords: string[][],
        readonly labels: string[][],
        readonly negativeLabels: string[][],
        readonly owners: string[][],
        readonly negativeOwners: string[][]
    ) {
        this.query = query ?? ''
        if (query == null) {
            this.query = this.toString()
        }
    }

    /** Return a list of {@link AssetQueryTerm}s found in the raw user input string. */
    static terms(query: string): AssetQueryTerm[] {
        const terms: AssetQueryTerm[] = []
        for (const [, tag, valuesRaw] of query.trim().matchAll(this.termsRegex)) {
            // Ignore values with a tag but without a value.
            if (valuesRaw != null && valuesRaw !== '') {
                const values = valuesRaw.match(AssetQuery.valuesRegex) ?? []
                terms.push({
                    tag: tag ?? null,
                    values: values.map(value =>
                        AssetQuery.jsonValueRegex.test(value)
                            ? String(JSON.parse(value.endsWith('"') ? value : value + '"'))
                            : value
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
            .map(value => (AssetQuery.plainValueRegex.test(value) ? value : JSON.stringify(value)))
            .join(',')
        return tagSegment + valueSegment
    }

    /** Create an {@link AssetQuery} from a raw user input string. */
    static fromString(query: string): AssetQuery {
        const terms = AssetQuery.terms(query)
        const keywords: string[][] = []
        const negativeKeywords: string[][] = []
        const labels: string[][] = []
        const negativeLabels: string[][] = []
        const owners: string[][] = []
        const negativeOwners: string[][] = []
        const tagNameToSet: Record<string, string[][]> = {
            // This is a dictionary, not an object.
            /* eslint-disable @typescript-eslint/naming-convention */
            '': keywords,
            name: keywords,
            '-name': negativeKeywords,
            label: labels,
            '-label': negativeLabels,
            owner: owners,
            '-owner': negativeOwners,
            /* eslint-enable @typescript-eslint/naming-convention */
        }
        for (const term of terms) {
            const set = term.tag == null ? keywords : tagNameToSet[term.tag]
            set?.push(term.values)
        }
        return new AssetQuery(
            query,
            keywords,
            negativeKeywords,
            labels,
            negativeLabels,
            owners,
            negativeOwners
        )
    }

    /** Return a new array of terms, after applying the given updates. */
    static updatedTerms(
        original: string[][],
        toAdd: string[][] | null,
        toRemove: string[][] | null
    ) {
        toAdd = toAdd?.filter(term => term.length !== 0) ?? null
        toRemove = toRemove?.filter(term => term.length !== 0) ?? null
        toAdd = toAdd?.length === 0 ? null : toAdd
        toRemove = toRemove?.length === 0 ? null : toRemove
        if (toAdd == null && (toRemove == null || original.length === 0)) {
            return null
        } else {
            let changed = false
            let terms = original
            if (toAdd != null) {
                const termsAfterAdditions = [
                    ...terms,
                    ...toAdd.filter(otherTerm =>
                        terms.every(
                            term => !array.shallowEqual([...term].sort(), [...otherTerm].sort())
                        )
                    ),
                ]
                if (termsAfterAdditions.length !== terms.length) {
                    terms = termsAfterAdditions
                    changed = true
                }
            }
            if (toRemove != null) {
                const termsAfterRemovals = terms.filter(
                    term =>
                        toRemove?.every(
                            otherTerm =>
                                !array.shallowEqual([...term].sort(), [...otherTerm].sort())
                        )
                )
                if (termsAfterRemovals.length !== terms.length) {
                    terms = termsAfterRemovals
                    changed = true
                }
            }
            return !changed ? null : terms
        }
    }

    /** Return a new array of terms, after applying the given updates to the last term. */
    static updatedLastTerm(
        original: string[][],
        toAdd: string[] | null,
        toRemove: string[] | null
    ) {
        toAdd = toAdd?.filter(term => term.length !== 0) ?? null
        toRemove = toRemove?.filter(term => term.length !== 0) ?? null
        toAdd = toAdd?.length === 0 ? null : toAdd
        toRemove = toRemove?.length === 0 ? null : toRemove
        let lastTerm = original[original.length - 1]
        if (toAdd == null && (toRemove == null || lastTerm == null || lastTerm.length === 0)) {
            return null
        } else {
            lastTerm ??= []
            let changed = false
            if (toAdd != null) {
                const lastTermAfterAdditions = [
                    ...lastTerm,
                    ...toAdd.filter(word => lastTerm?.includes(word) === false),
                ]
                if (lastTermAfterAdditions.length !== lastTerm.length) {
                    lastTerm = lastTermAfterAdditions
                    changed = true
                }
            }
            if (toRemove != null) {
                const lastTermAfterRemovals = lastTerm.filter(
                    word => toRemove?.includes(word) === false
                )
                if (lastTermAfterRemovals.length !== lastTerm.length) {
                    lastTerm = lastTermAfterRemovals
                    changed = true
                }
            }
            return !changed
                ? null
                : original.slice(0, -1).concat(lastTerm.length !== 0 ? [lastTerm] : [])
        }
    }

    /** Return a new array of terms, after applying the given updates to the last term. */
    static updatedEveryTerm(
        original: string[][],
        toAdd: string[] | null,
        toRemove: string[] | null
    ) {
        toAdd = toAdd?.filter(term => term.length !== 0) ?? null
        toRemove = toRemove?.filter(term => term.length !== 0) ?? null
        toAdd = toAdd?.length === 0 ? null : toAdd
        toRemove = toRemove?.length === 0 ? null : toRemove
        if (toAdd == null && (toRemove == null || original.length === 0)) {
            return null
        } else {
            const newTerms: string[][] = []
            let changed = false
            for (const term of original) {
                let newTerm = term
                if (toAdd != null) {
                    const termAfterAdditions = [
                        ...newTerm,
                        ...toAdd.filter(word => newTerm.includes(word) === false),
                    ]
                    if (termAfterAdditions.length !== newTerm.length) {
                        newTerm = termAfterAdditions
                        changed = true
                    }
                }
                if (toRemove != null) {
                    const termAfterRemovals = newTerm.filter(
                        word => toRemove?.includes(word) === false
                    )
                    if (termAfterRemovals.length !== newTerm.length) {
                        newTerm = termAfterRemovals
                        changed = true
                    }
                }
                if (newTerm.length !== 0) {
                    newTerms.push(newTerm)
                }
            }
            return !changed ? null : newTerms
        }
    }

    /** Return a new {@link AssetQuery} with the specified terms added,
     * or itself if there are no terms to add. */
    add(values: Partial<AssetQueryData>): AssetQuery {
        const updates: Partial<AssetQueryData> = {}
        for (const key of AssetQuery.dataKeys) {
            const update = AssetQuery.updatedTerms(this[key], values[key] ?? null, null)
            if (update != null) {
                updates[key] = update
            }
        }
        if (Object.keys(updates).length === 0) {
            return this
        } else {
            return new AssetQuery(
                null,
                updates.keywords ?? this.keywords,
                updates.negativeKeywords ?? this.negativeKeywords,
                updates.labels ?? this.labels,
                updates.negativeLabels ?? this.negativeLabels,
                updates.owners ?? this.owners,
                updates.negativeOwners ?? this.negativeOwners
            )
        }
    }

    /** Return a new {@link AssetQuery} with the specified terms deleted,
     * or itself if there are no terms to delete. */
    delete(values: Partial<AssetQueryData>): AssetQuery {
        const updates: Partial<AssetQueryData> = {}
        for (const key of AssetQuery.dataKeys) {
            const update = AssetQuery.updatedTerms(this[key], null, values[key] ?? null)
            if (update != null) {
                updates[key] = update
            }
        }
        if (Object.keys(updates).length === 0) {
            return this
        } else {
            return new AssetQuery(
                null,
                updates.keywords ?? this.keywords,
                updates.negativeKeywords ?? this.negativeKeywords,
                updates.labels ?? this.labels,
                updates.negativeLabels ?? this.negativeLabels,
                updates.owners ?? this.owners,
                updates.negativeOwners ?? this.negativeOwners
            )
        }
    }

    /** Return a new {@link AssetQuery} with the specified words added to the last term
     * with the matching tag, or itself if there are no terms to add. */
    addToLastTerm(values: Partial<AssetQueryLastTermData>): AssetQuery {
        const updates: Partial<AssetQueryData> = {}
        for (const key of AssetQuery.dataKeys) {
            const update = AssetQuery.updatedLastTerm(this[key], values[key] ?? null, null)
            if (update != null) {
                updates[key] = update
            }
        }
        if (Object.keys(updates).length === 0) {
            return this
        } else {
            return new AssetQuery(
                null,
                updates.keywords ?? this.keywords,
                updates.negativeKeywords ?? this.negativeKeywords,
                updates.labels ?? this.labels,
                updates.negativeLabels ?? this.negativeLabels,
                updates.owners ?? this.owners,
                updates.negativeOwners ?? this.negativeOwners
            )
        }
    }

    /** Return a new {@link AssetQuery} with the specified terms deleted from the last term
     * with the matching tag, or itself if there are no terms to delete. */
    deleteFromLastTerm(values: Partial<AssetQueryLastTermData>): AssetQuery {
        const updates: Partial<AssetQueryData> = {}
        for (const key of AssetQuery.dataKeys) {
            const update = AssetQuery.updatedLastTerm(this[key], null, values[key] ?? null)
            if (update != null) {
                updates[key] = update
            }
        }
        if (Object.keys(updates).length === 0) {
            return this
        } else {
            return new AssetQuery(
                null,
                updates.keywords ?? this.keywords,
                updates.negativeKeywords ?? this.negativeKeywords,
                updates.labels ?? this.labels,
                updates.negativeLabels ?? this.negativeLabels,
                updates.owners ?? this.owners,
                updates.negativeOwners ?? this.negativeOwners
            )
        }
    }

    /** Return a new {@link AssetQuery} with the specified words added to every term
     * with the matching tag, or itself if there are no terms to add.
     * Note that this makes little sense to use, but is added for symmetry with
     * {@link AssetQuery.deleteFromEveryTerm}. */
    addToEveryTerm(values: Partial<AssetQueryLastTermData>): AssetQuery {
        const updates: Partial<AssetQueryData> = {}
        for (const key of AssetQuery.dataKeys) {
            const update = AssetQuery.updatedEveryTerm(this[key], values[key] ?? null, null)
            if (update != null) {
                updates[key] = update
            }
        }
        if (Object.keys(updates).length === 0) {
            return this
        } else {
            return new AssetQuery(
                null,
                updates.keywords ?? this.keywords,
                updates.negativeKeywords ?? this.negativeKeywords,
                updates.labels ?? this.labels,
                updates.negativeLabels ?? this.negativeLabels,
                updates.owners ?? this.owners,
                updates.negativeOwners ?? this.negativeOwners
            )
        }
    }

    /** Return a new {@link AssetQuery} with the specified terms deleted from the last term
     * with the matching tag, or itself if there are no terms to delete. */
    deleteFromEveryTerm(values: Partial<AssetQueryLastTermData>): AssetQuery {
        const updates: Partial<AssetQueryData> = {}
        for (const key of AssetQuery.dataKeys) {
            const update = AssetQuery.updatedEveryTerm(this[key], null, values[key] ?? null)
            if (update != null) {
                updates[key] = update
            }
        }
        if (Object.keys(updates).length === 0) {
            return this
        } else {
            return new AssetQuery(
                null,
                updates.keywords ?? this.keywords,
                updates.negativeKeywords ?? this.negativeKeywords,
                updates.labels ?? this.labels,
                updates.negativeLabels ?? this.negativeLabels,
                updates.owners ?? this.owners,
                updates.negativeOwners ?? this.negativeOwners
            )
        }
    }

    /** Returns a string representation usable in the search bar. */
    toString() {
        const segments: string[] = []
        for (const [key, tag] of AssetQuery.tagNames) {
            for (const values of this[key]) {
                segments.push(AssetQuery.termToString({ tag, values }))
            }
        }
        return segments.join(' ')
    }
}

/** Tries to cycle the label between:
 * - not present
 * - present as a positive search, and
 * - present as a negative search. */
export function toggleLabel(query: AssetQuery, label: string, fromLastTerm = false) {
    let newQuery = query
    if (fromLastTerm) {
        newQuery = newQuery.deleteFromLastTerm({ negativeLabels: [label] })
        if (newQuery === query) {
            newQuery = newQuery.deleteFromLastTerm({ labels: [label] })
            newQuery = newQuery.addToLastTerm(
                newQuery === query ? { labels: [label] } : { negativeLabels: [label] }
            )
        }
    } else {
        newQuery = newQuery.delete({ negativeLabels: [[label]] })
        if (newQuery === query) {
            newQuery = newQuery.delete({ labels: [[label]] })
            newQuery = newQuery.add(
                newQuery === query ? { labels: [[label]] } : { negativeLabels: [[label]] }
            )
        }
    }
    return newQuery
}
