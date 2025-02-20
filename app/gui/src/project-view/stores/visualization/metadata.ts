import type { VisualizationModule } from '@/stores/visualization/runtimeTypes'
import type { Opt } from '@/util/data/opt'
import { ReactiveDb, ReactiveIndex } from '@/util/database/reactiveDb'
import type { VisualizationIdentifier } from 'ydoc-shared/yjsModel'

export type VisualizationMetadata = Pick<VisualizationModule, 'name' | 'inputType' | 'icon'>

function getTypesFromUnion(inputType: Opt<string>) {
  return inputType?.split('|').map((type) => type.trim()) ?? ['Any']
}

declare const visualizationIdBrand: unique symbol
export type VisualizationId = string & { [visualizationIdBrand]: never }

/** TODO: Add docs */
export function toVisualizationId(meta: VisualizationIdentifier) {
  return JSON.stringify({
    // All fields MUST be explicitly written so that the order is consistent.
    module: {
      kind: meta.module.kind,
      name: meta.module.kind === 'Library' ? meta.module.name : undefined,
    },
    name: meta.name,
  }) as VisualizationId
}

/** TODO: Add docs */
export function fromVisualizationId(key: VisualizationId): VisualizationIdentifier {
  return JSON.parse(key)
}

/** TODO: Add docs */
export class VisualizationMetadataDb extends ReactiveDb<VisualizationId, VisualizationMetadata> {
  visualizationIdToType = new ReactiveIndex(this, (key, metadata) =>
    getTypesFromUnion(metadata.inputType).map((type) => [key, type]),
  )
}
