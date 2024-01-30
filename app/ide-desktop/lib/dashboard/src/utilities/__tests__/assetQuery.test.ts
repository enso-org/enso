/** @file Tests for {@link assetQuery.AssetQuery}. */
import * as v from 'vitest'

import * as assetQuery from '#/utilities/assetQuery'

v.test.each([
  { query: '' },
  { query: 'name:', names: [[]] },
  { query: '-name:', negativeNames: [[]] },
  { query: 'label:', labels: [[]] },
  { query: '-label:', negativeLabels: [[]] },
  { query: 'owner:', owners: [[]] },
  { query: '-owner:', negativeOwners: [[]] },
  { query: '"', keywords: [['']] },
  { query: '""', keywords: [['']] },
  { query: 'a', keywords: [['a']] },
  { query: 'a b', keywords: [['a'], ['b']] },
  { query: '"a" "b"', keywords: [['a'], ['b']] },
  { query: 'a,b', keywords: [['a', 'b']] },
  { query: '"a","b"', keywords: [['a', 'b']] },
  { query: '-:a', negativeKeywords: [['a']] },
  { query: '-:a,b', negativeKeywords: [['a', 'b']] },
  { query: 'name:a,b', names: [['a', 'b']] },
  { query: '-name:a', negativeNames: [['a']] },
  { query: '-name:a,b', negativeNames: [['a', 'b']] },
  { query: 'label:a', labels: [['a']] },
  { query: '-label:a', negativeLabels: [['a']] },
  { query: 'owner:a', owners: [['a']] },
  { query: '-owner:a', negativeOwners: [['a']] },
  { query: 'no:a', nos: [['a']] },
  { query: '-no:a', negativeNos: [['a']] },
  { query: 'has:a', negativeNos: [['a']] },
  { query: '-has:a', nos: [['a']] },
  // Ensure that invalid queries are parsed reasonably
  { query: '-label', keywords: [['-label']] },
  { query: '"a" "b', keywords: [['a'], ['b']] },
  { query: '"a","b', keywords: [['a', 'b']] },
  { query: '"a""b"', keywords: [['a', 'b']] },
  { query: '"a""b', keywords: [['a', 'b']] },
  { query: '"a"b"', keywords: [['a', 'b"']] },
])(
  'AssetQuery.fromString',
  ({
    query,
    keywords,
    negativeKeywords,
    names,
    negativeNames,
    labels,
    negativeLabels,
    owners,
    negativeOwners,
    nos,
    negativeNos,
  }) => {
    const parsed = assetQuery.AssetQuery.fromString(query)
    v.expect(parsed.keywords, `Keywords in '${query}'`).toEqual(keywords ?? [])
    v.expect(parsed.negativeKeywords, `Negative keywords in '${query}'`).toEqual(
      negativeKeywords ?? []
    )
    v.expect(parsed.names, `Names in '${query}'`).toEqual(names ?? [])
    v.expect(parsed.negativeNames, `Negative names in '${query}'`).toEqual(negativeNames ?? [])
    v.expect(parsed.labels, `Labels in '${query}'`).toEqual(labels ?? [])
    v.expect(parsed.negativeLabels, `Negative labels in '${query}'`).toEqual(negativeLabels ?? [])
    v.expect(parsed.owners, `Owners in '${query}'`).toEqual(owners ?? [])
    v.expect(parsed.negativeOwners, `Negative owners in '${query}'`).toEqual(negativeOwners ?? [])
    v.expect(parsed.nos, `Nos in '${query}'`).toEqual(nos ?? [])
    v.expect(parsed.negativeNos, `Negative nos in '${query}'`).toEqual(negativeNos ?? [])
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
  // Edge cases. The exact result should not matter, as long as it is reasonable.
  { query: 'a a', updates: { keywords: [['a']] }, newQuery: '' },
])('AssetQuery#delete', ({ query, updates, newQuery }) => {
  const parsed = assetQuery.AssetQuery.fromString(query)
  v.expect(
    parsed.delete(updates).toString(),
    `'${query}' with ${JSON.stringify(updates)} deleted should be '${newQuery}'`
  ).toBe(newQuery)
})

v.test.each([{ query: 'a', updates: { keywords: ['b'] }, newQuery: 'a,b' }])(
  'AssetQuery#addToLastTerm',
  ({ query, updates, newQuery }) => {
    const parsed = assetQuery.AssetQuery.fromString(query)
    v.expect(
      parsed.addToLastTerm(updates).toString(),
      `'${query}' with ${JSON.stringify(updates)} added should be '${newQuery}'`
    ).toBe(newQuery)
  }
)

v.test.each([
  { query: 'a b', updates: { keywords: ['b'] }, newQuery: 'a' },
  { query: 'a b', updates: { keywords: ['a'] }, newQuery: 'a b' },
  { query: 'a b,c', updates: { keywords: ['c'] }, newQuery: 'a b' },
  { query: 'a b,c', updates: { keywords: ['b', 'd', 'e', 'f'] }, newQuery: 'a c' },
  { query: 'a b,c', updates: { keywords: ['b', 'c'] }, newQuery: 'a' },
  { query: 'a', updates: { keywords: ['a'] }, newQuery: '' },
  { query: 'a b c', updates: { keywords: ['b', 'c'] }, newQuery: 'a b' },
])('AssetQuery#deleteFromLastTerm', ({ query, updates, newQuery }) => {
  const parsed = assetQuery.AssetQuery.fromString(query)
  v.expect(
    parsed.deleteFromLastTerm(updates).toString(),
    `'${query}' with ${JSON.stringify(updates)} deleted should be '${newQuery}'`
  ).toBe(newQuery)
})

v.test.each([
  { query: 'a b a', updates: { keywords: ['b'] }, newQuery: 'a a' },
  { query: 'a b a', updates: { keywords: ['a'] }, newQuery: 'b' },
  { query: 'a b,c', updates: { keywords: ['c'] }, newQuery: 'a b' },
  { query: 'a b,c', updates: { keywords: ['b', 'd', 'e', 'f'] }, newQuery: 'a c' },
  { query: 'a b,c', updates: { keywords: ['b', 'c'] }, newQuery: 'a' },
  { query: 'b,c a', updates: { keywords: ['b', 'c'] }, newQuery: 'a' },
  { query: 'a', updates: { keywords: ['a'] }, newQuery: '' },
  { query: 'a b c', updates: { keywords: ['b', 'c'] }, newQuery: 'a' },
])('AssetQuery#deleteFromEveryTerm', ({ query, updates, newQuery }) => {
  const parsed = assetQuery.AssetQuery.fromString(query)
  v.expect(
    parsed.deleteFromEveryTerm(updates).toString(),
    `'${query}' with ${JSON.stringify(updates)} deleted should be '${newQuery}'`
  ).toBe(newQuery)
})
