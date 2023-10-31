import { fc, test } from '@fast-check/vitest'
import { expect } from 'vitest'

import { fromVisualizationId, toVisualizationId } from '@/stores/visualization/metadata'
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
