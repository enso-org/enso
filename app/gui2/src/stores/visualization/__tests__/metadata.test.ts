import { fc, test } from '@fast-check/vitest'
import { expect } from 'vitest'

import {
  fromVisualizationId,
  toVisualizationId,
  VisualizationMetadataDb,
} from '@/stores/visualization/metadata'
import type { VisualizationIdentifier } from 'shared/yjsModel'

test.prop({
  kind: fc.oneof(
    fc.constant<'Builtin'>('Builtin'),
    fc.constant<'CurrentProject'>('CurrentProject'),
    fc.constant<'Library'>('Library'),
  ),
  libraryName: fc.string(),
  name: fc.string(),
})(
  '`toVisualizationId` and `fromVisualizationId` can be round-tripped',
  ({ kind, libraryName, name }) => {
    const ident: VisualizationIdentifier = {
      module: kind === 'Library' ? { kind, name: libraryName } : { kind },
      name,
    }
    const id = toVisualizationId(ident)
    const reconstructedIdent = fromVisualizationId(id)
    expect(ident).toEqual(reconstructedIdent)
    expect(id).toEqual(toVisualizationId(reconstructedIdent))
  },
)

test('metadata index', () => {
  const db = new VisualizationMetadataDb()
  const a = toVisualizationId({ module: { kind: 'Builtin' }, name: 'a' })
  const b = toVisualizationId({ module: { kind: 'Builtin' }, name: 'b' })
  db.set(a, { name: 'a', inputType: 'B | C' })
  db.set(b, { name: 'b', inputType: 'C | D | E' })
  expect(db.types.lookup(a)).toEqual(new Set(['B', 'C']))
  expect(db.types.lookup(b)).toEqual(new Set(['C', 'D', 'E']))
  expect(db.types.reverseLookup('B')).toEqual(new Set([a]))
  expect(db.types.reverseLookup('C')).toEqual(new Set([a, b]))
  expect(db.types.reverseLookup('D')).toEqual(new Set([b]))
  expect(db.types.reverseLookup('E')).toEqual(new Set([b]))

  db.delete(b)
  expect(db.types.lookup(a)).toEqual(new Set(['B', 'C']))
  expect(db.types.lookup(b)).toEqual(new Set())
  expect(db.types.reverseLookup('B')).toEqual(new Set([a]))
  expect(db.types.reverseLookup('C')).toEqual(new Set([a]))
  expect(db.types.reverseLookup('D')).toEqual(new Set())
  expect(db.types.reverseLookup('E')).toEqual(new Set())
})
