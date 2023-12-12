import { createContextStore } from '@/providers'
import type { Icon } from '@/util/iconName'
import type { URLString } from '@/util/urlString'
import { Vec2 } from '@/util/vec2'
import type { VisualizationIdentifier } from 'shared/yjsModel'
import { reactive } from 'vue'

export interface VisualizationConfig {
  background?: string
  /** Possible visualization types that can be switched to. */
  readonly types: Iterable<VisualizationIdentifier>
  readonly currentType: VisualizationIdentifier
  readonly icon: Icon | URLString | undefined
  readonly isCircularMenuVisible: boolean
  readonly nodeSize: Vec2
  readonly scale: number
  isBelowToolbar: boolean
  width: number | null
  height: number
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
