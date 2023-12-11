/** @file Parsing and representation of the search query. */

// ==================
// === AssetQuery ===
// ==================

/** An {@link AssetQuery}, without the query and methods. */
interface AssetQueryData extends Omit<AssetQuery, 'add' | 'query' | 'remove'> {}

/** An individual segment of a query string input to {@link AssetQuery}. */
interface AssetQueryTerm {
    tag: string | null
    value: string
}

/** Parsing and representation of the search query. */
export class AssetQuery {
    static termsRegex =
        // Control characters must be handled, in order to follow the JSON spec.
        // eslint-disable-next-line no-control-regex
        /(?:([^\s:]*):)?(?:("(?:[^\0-\x1f\\"]|\\[\\/bfnrt"]|\\u[0-9a-fA-F]{4})*")|([^\s"]\S*|))/g
    static plainValueRegex = /^(?:|[^"]\S*)$/u

    /** Create an {@link AssetQuery}. */
    constructor(
        readonly query: string,
        readonly keywords: string[],
        readonly labels: string[]
    ) {}

    /** Return a list of {@link AssetQueryTerm}s found in the raw user input string. */
    static terms(query: string): AssetQueryTerm[] {
        const terms: AssetQueryTerm[] = []
        for (const [, tag, jsonValue, plainValue] of query.trim().matchAll(this.termsRegex)) {
            if (tag != null || plainValue == null || plainValue !== '') {
                terms.push({
                    tag: tag ?? null,
                    value: jsonValue != null ? String(JSON.parse(jsonValue)) : plainValue ?? '',
                })
            }
        }
        return terms
    }

    /** Convert an {@link AssetQueryTerm} to a string usable in a raw user input string. */
    static termToString(term: AssetQueryTerm) {
        const tagSegment = term.tag == null ? '' : term.tag + ':'
        const valueSegment = this.plainValueRegex.test(term.value)
            ? term.value
            : JSON.stringify(term.value)
        return tagSegment + valueSegment
    }

    /** Create an {@link AssetQuery} from a raw user input string. */
    static fromString(query: string): AssetQuery {
        const terms = AssetQuery.terms(query)
        const keywords = terms
            .filter(term => term.tag == null || term.tag === '')
            .map(term => term.value)
        const labels = terms
            .filter(term => term.tag?.toLowerCase() === 'label')
            .map(term => term.value)
        return new AssetQuery(query, keywords, labels)
    }

    /** Return a new {@link AssetQuery} with the specified terms added,
     * or itself if there are no terms to remove. */
    add(values: Partial<AssetQueryData>): AssetQuery {
        const { keywords, labels } = values
        const noKeywords = !keywords || keywords.length === 0
        const noLabels = !labels || labels.length === 0
        if (noKeywords && noLabels) {
            return this
        } else {
            const newKeywords = this.keywords
            let addedKeywords: string[] = []
            if (!noKeywords) {
                const keywordsSet = new Set(this.keywords)
                addedKeywords = keywords.filter(keyword => !keywordsSet.has(keyword))
                newKeywords.push(...addedKeywords)
            }
            const newLabels = this.labels
            let addedLabels: string[] = []
            if (!noLabels) {
                const labelsSet = new Set(this.labels)
                addedLabels = labels.filter(keyword => !labelsSet.has(keyword))
                newLabels.push(...addedLabels)
            }
            const newQuery =
                this.query +
                (this.query === '' ? '' : ' ') +
                [
                    ...addedKeywords.map(keyword =>
                        AssetQuery.termToString({ tag: null, value: keyword })
                    ),
                    ...addedLabels.map(label =>
                        AssetQuery.termToString({ tag: 'label', value: label })
                    ),
                ].join(' ')
            return new AssetQuery(newQuery, newKeywords, newLabels)
        }
    }

    /** Return a new {@link AssetQuery} with the specified terms removed,
     * or itself if there are no terms to remove. */
    delete(values: Partial<AssetQueryData>): AssetQuery {
        const { keywords, labels } = values
        const noKeywords = !keywords || keywords.length === 0
        const noLabels = !labels || labels.length === 0
        if (noKeywords && noLabels) {
            return this
        } else {
            let newKeywords = this.keywords
            const keywordsSet = new Set(keywords ?? [])
            if (!noKeywords) {
                newKeywords = newKeywords.filter(keyword => !keywordsSet.has(keyword))
            }
            let newLabels = this.labels
            const labelsSet = new Set(labels ?? [])
            if (!noLabels) {
                newLabels = newLabels.filter(label => !labelsSet.has(label))
            }
            if (
                newKeywords.length === this.keywords.length &&
                newLabels.length === this.labels.length
            ) {
                return this
            } else {
                const newQuery = AssetQuery.terms(this.query)
                    .filter(term => {
                        switch (term.tag?.toLowerCase() ?? null) {
                            case null:
                            case '': {
                                return !keywordsSet.has(term.value)
                            }
                            case 'label': {
                                return !labelsSet.has(term.value)
                            }
                            default: {
                                return true
                            }
                        }
                    })
                    .map(term => AssetQuery.termToString(term))
                    .join(' ')
                return new AssetQuery(newQuery, newKeywords, newLabels)
            }
        }
    }
}
