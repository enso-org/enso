/** Translation of `yjsModel` types to and from the `fileFormat` representation. */

import * as json from 'lib0/json'
import { ExternalId, IdMap, sourceRangeFromKey } from 'ydoc-shared/yjsModel'
import * as fileFormat from './fileFormat'

/** Convert a JSON string to an {@link IdMap}. */
export function deserializeIdMap(idMapJson: string): IdMap {
  const idMapMeta = fileFormat.tryParseIdMapOrFallback(idMapJson)
  const idMap = new IdMap()
  for (const [{ index, size }, id] of idMapMeta) {
    const range = [index.value, index.value + size.value]
    if (typeof range[0] !== 'number' || typeof range[1] !== 'number') {
      console.error(`Invalid range for id ${id}:`, range)
      continue
    }
    idMap.insertKnownId([index.value, index.value + size.value], id as ExternalId)
  }
  return idMap
}

/** Convert an {@link IdMap} to a JSON string. */
export function serializeIdMap(map: IdMap): string {
  map.validate()
  return json.stringify(idMapToArray(map))
}

/** Convert an {@link IdMap} to an array of {@link fileFormat.IdMapEntry}. */
export function idMapToArray(map: IdMap): fileFormat.IdMapEntry[] {
  const entries: fileFormat.IdMapEntry[] = []
  map.entries().forEach(([rangeBuffer, id]) => {
    const decoded = sourceRangeFromKey(rangeBuffer)
    const index = decoded[0]
    const endIndex = decoded[1]
    if (index == null || endIndex == null) return
    const size = endIndex - index
    entries.push([{ index: { value: index }, size: { value: size } }, id])
  })
  entries.sort(idMapCmp)
  return entries
}

/** Compare two {@link fileFormat.IdMapEntry}. */
function idMapCmp(a: fileFormat.IdMapEntry, b: fileFormat.IdMapEntry): number {
  const val1 = a[0]?.index?.value ?? 0
  const val2 = b[0]?.index?.value ?? 0
  if (val1 === val2) {
    const size1 = a[0]?.size.value ?? 0
    const size2 = b[0]?.size.value ?? 0
    return size1 - size2
  }
  return val1 - val2
}
