/** @file Tests for {@link AssetQuery}. */
import * as v from 'vitest'

import AssetQuery from '#/utilities/AssetQuery'

v.test.each([
  { query: '' },
  { query: 'name:', names: [] },
  { query: 'label:', labels: [] },
  { query: 'owner:', owners: [] },
  { query: '"', keywords: [''] },
  { query: '""', keywords: [''] },
  { query: 'a', keywords: ['a'] },
  { query: 'a b', keywords: ['a', 'b'] },
  { query: '"a" "b"', keywords: ['a', 'b'] },
  { query: 'a,b', keywords: ['a', 'b'] },
  { query: '"a","b"', keywords: ['a', 'b'] },
  { query: 'name:a,b', names: ['a', 'b'] },
  { query: 'label:a', labels: ['a'] },
  { query: 'owner:a', owners: ['a'] },
  // Ensure that invalid queries are parsed reasonably
  { query: '-label', keywords: ['-label'] },
  { query: '"a" "b', keywords: ['a', 'b'] },
  { query: '"a","b', keywords: ['a', 'b'] },
  { query: '"a""b"', keywords: ['a', 'b'] },
  { query: '"a""b', keywords: ['a', 'b'] },
  { query: '"a"b"', keywords: ['a', 'b"'] },
])('AssetQuery.fromString', ({ query, keywords, names, labels, owners }) => {
  const parsed = AssetQuery.fromString(query)
  v.expect(parsed.keywords, `Keywords in '${query}'`).toEqual(keywords ?? [])
  v.expect(parsed.names, `Names in '${query}'`).toEqual(names ?? [])
  v.expect(parsed.labels, `Labels in '${query}'`).toEqual(labels ?? [])
  v.expect(parsed.owners, `Owners in '${query}'`).toEqual(owners ?? [])
})

v.test.each([{ query: 'a', key: 'keywords' as const, value: ['b'], newQuery: 'a b' }])(
  'AssetQuery#add',
  ({ query, key, value, newQuery }) => {
    const parsed = AssetQuery.fromString(query)
    v.expect(
      parsed.add(key, value).toString(),
      `'${query}' with ${key}=${JSON.stringify(value)} added should be '${newQuery}'`,
    ).toBe(newQuery)
  },
)

v.test.each([
  { query: 'a b', key: 'keywords' as const, value: ['b'], newQuery: 'a' },
  { query: 'a', key: 'keywords' as const, value: ['a'], newQuery: '' },
  // Edge cases. The exact result should not matter, as long as it is reasonable.
  { query: 'a a', key: 'keywords' as const, value: ['a'], newQuery: '' },
])('AssetQuery#delete', ({ query, key, value, newQuery }) => {
  const parsed = AssetQuery.fromString(query)
  v.expect(
    parsed.delete(key, value).toString(),
    `'${query}' with ${key}=${JSON.stringify(value)} deleted should be '${newQuery}'`,
  ).toBe(newQuery)
})
