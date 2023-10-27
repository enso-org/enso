import type { Vec2 } from '@/util/vec2'
import type { VisualizationIdentifier } from 'shared/yjsModel'
import { reactive } from 'vue'
import { createContextStore } from '.'

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

export { provideFn as provideVisualizationConfig }
const { provideFn, injectFn } = createContextStore(
  'Visualization config',
  reactive<VisualizationConfig>,
)

// The visualization config public API should not expose the `allowMissing` parameter. It should
// look like an ordinary vue composable.

export function useVisualizationConfig() {
  return injectFn()
}
