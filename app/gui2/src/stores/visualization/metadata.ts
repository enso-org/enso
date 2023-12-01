import type { VisualizationModule } from '@/stores/visualization/runtimeTypes'
import { ReactiveDb, ReactiveIndex } from '@/util/database/reactiveDb'
import type { Opt } from '@/util/opt'
import type { VisualizationIdentifier } from 'shared/yjsModel'

export interface VisualizationMetadata
  extends Pick<VisualizationModule, 'name' | 'inputType' | 'icon'> {}

function getTypesFromUnion(inputType: Opt<string>) {
  return inputType?.split('|').map((type) => type.trim()) ?? ['Any']
}

declare const visualizationIdBrand: unique symbol
export type VisualizationId = string & { [visualizationIdBrand]: never }

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

export function fromVisualizationId(key: VisualizationId): VisualizationIdentifier {
  return JSON.parse(key)
}

export class VisualizationMetadataDb extends ReactiveDb<VisualizationId, VisualizationMetadata> {
  visualizationIdToType = new ReactiveIndex(this, (key, metadata) =>
    getTypesFromUnion(metadata.inputType).map((type) => [key, type]),
  )
}
