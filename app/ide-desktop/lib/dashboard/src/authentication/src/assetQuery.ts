/** @file Parsing and representation of the search query. */

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
interface AssetQueryData extends Mutable<Omit<AssetQuery, 'add' | 'delete' | 'query'>> {}

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
            // FIXME:
            return original
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
