/** @file Tests for {@link assetQuery.AssetQuery}. */
import * as v from 'vitest'

import * as assetQuery from '../assetQuery'

v.test.each([
    { query: '' },
    { query: 'name:' },
    { query: '-name:' },
    { query: 'label:' },
    { query: '-label:' },
    { query: 'owner:' },
    { query: '-owner:' },
    { query: 'a', keywords: [['a']] },
    { query: 'a b', keywords: [['a'], ['b']] },
    { query: '"a" "b"', keywords: [['a'], ['b']] },
    { query: 'a,b', keywords: [['a', 'b']] },
    { query: '"a","b"', keywords: [['a', 'b']] },
    { query: 'name:a,b', keywords: [['a', 'b']] },
    { query: '-name:a', negativeKeywords: [['a']] },
    { query: '-name:a,b', negativeKeywords: [['a', 'b']] },
    { query: 'label:a', labels: [['a']] },
    { query: '-label:a', negativeLabels: [['a']] },
    { query: 'owner:a', owners: [['a']] },
    { query: '-owner:a', negativeOwners: [['a']] },
    // Ensure that invalid queries are parsed reasonably
    { query: '-label', keywords: [['-label']] },
    { query: '"a" "b', keywords: [['a'], ['b']] },
    { query: '"a","b', keywords: [['a', 'b']] },
    { query: '"a""b"', keywords: [['a', 'b']] },
    { query: '"a""b', keywords: [['a', 'b']] },
    { query: '"a"b"', keywords: [['a', 'b"']] },
])(
    'AssetQuery.fromString',
    ({ query, keywords, negativeKeywords, labels, negativeLabels, owners, negativeOwners }) => {
        const parsed = assetQuery.AssetQuery.fromString(query)
        v.expect(parsed.keywords, `Keywords in '${query}'`).toEqual(keywords ?? [])
        v.expect(parsed.negativeKeywords, `Negative keywords in '${query}'`).toEqual(
            negativeKeywords ?? []
        )
        v.expect(parsed.labels, `Labels in '${query}'`).toEqual(labels ?? [])
        v.expect(parsed.negativeLabels, `Negative labels in '${query}'`).toEqual(
            negativeLabels ?? []
        )
        v.expect(parsed.owners, `Owners in '${query}'`).toEqual(owners ?? [])
        v.expect(parsed.negativeOwners, `Negative owners in '${query}'`).toEqual(
            negativeOwners ?? []
        )
    }
)

v.test.each([{ query: 'a', updates: { keywords: [['b']] }, newQuery: 'a b' }])(
    'AssetQuery#add',
    ({ query, updates, newQuery }) => {
        const parsed = assetQuery.AssetQuery.fromString(query)
        v.expect(
            parsed.add(updates).toString(),
            `'${query}' with ${JSON.stringify(updates)} added should be '${newQuery}'`
        ).toBe(newQuery)
    }
)

v.test.each([
    { query: 'a b', updates: { keywords: [['b']] }, newQuery: 'a' },
    { query: 'a', updates: { keywords: [['a']] }, newQuery: '' },
    // Edge cases
    { query: 'a a', updates: { keywords: [['a']] }, newQuery: 'a' },
])('AssetQuery#delete', ({ query, updates, newQuery }) => {
    const parsed = assetQuery.AssetQuery.fromString(query)
    v.expect(
        parsed.delete(updates).toString(),
        `'${query}' with ${JSON.stringify(updates)} deleted should be '${newQuery}'`
    ).toBe(newQuery)
})
