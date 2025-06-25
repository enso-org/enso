import { fc, test } from '@fast-check/vitest'
import { expect } from 'vitest'

import {
  fromVisualizationId,
  toVisualizationId,
  VisualizationMetadataDb,
} from '@/stores/visualization/metadata'
import type { VisualizationIdentifier } from 'ydoc-shared/yjsModel'

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

function makePath(ident: string): string {
  return 'Standard.Base.' + ident
}

test('metadata index', () => {
  const db = new VisualizationMetadataDb()
  const a = toVisualizationId({ module: { kind: 'Builtin' }, name: 'a' })
  const b = toVisualizationId({ module: { kind: 'Builtin' }, name: 'b' })
  db.set(a, { name: 'a', inputType: 'Standard.Base.B | Standard.Base.C' })
  db.set(b, { name: 'b', inputType: 'Standard.Base.C | Standard.Base.D | Standard.Base.E' })
  expect(db.visualizationIdToType.lookup(a)).toEqual(new Set([makePath('B'), makePath('C')]))
  expect(db.visualizationIdToType.lookup(b)).toEqual(
    new Set([makePath('C'), makePath('D'), makePath('E')]),
  )
  expect(db.visualizationIdToType.reverseLookup(makePath('B'))).toEqual(new Set([a]))
  expect(db.visualizationIdToType.reverseLookup(makePath('C'))).toEqual(new Set([a, b]))
  expect(db.visualizationIdToType.reverseLookup(makePath('D'))).toEqual(new Set([b]))
  expect(db.visualizationIdToType.reverseLookup(makePath('E'))).toEqual(new Set([b]))

  db.delete(b)
  expect(db.visualizationIdToType.lookup(a)).toEqual(new Set([makePath('B'), makePath('C')]))
  expect(db.visualizationIdToType.lookup(b)).toEqual(new Set())
  expect(db.visualizationIdToType.reverseLookup(makePath('B'))).toEqual(new Set([a]))
  expect(db.visualizationIdToType.reverseLookup(makePath('C'))).toEqual(new Set([a]))
  expect(db.visualizationIdToType.reverseLookup(makePath('D'))).toEqual(new Set())
  expect(db.visualizationIdToType.reverseLookup(makePath('E'))).toEqual(new Set())
})
