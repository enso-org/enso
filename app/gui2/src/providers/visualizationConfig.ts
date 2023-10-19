import type { Vec2 } from '@/util/vec2'
import type { VisualizationIdentifier } from 'shared/yjsModel'
import { reactive } from 'vue'
import { createProvidable } from '.'

export interface VisualizationConfig {
  /** Possible visualization types that can be switched to. */
  background?: string
  readonly types: readonly VisualizationIdentifier[]
  readonly currentType: VisualizationIdentifier
  readonly isCircularMenuVisible: boolean
  readonly nodeSize: Vec2
  width: number | null
  height: number | null
  fullscreen: boolean
  hide: () => void
  updateType: (type: VisualizationIdentifier) => void
}

const { provideFn, useFn } = createProvidable(reactive<VisualizationConfig>)
export { provideFn as provideVisualizationConfig, useFn as useVisualizationConfig }
