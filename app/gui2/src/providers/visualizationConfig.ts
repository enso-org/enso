import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import { SavedSize } from '@/components/WithFullscreenMode.vue'
import { createContextStore } from '@/providers'
import type { URLString } from '@/util/data/urlString'
import { Vec2 } from '@/util/data/vec2'
import type { Icon } from '@/util/iconName'
import { reactive } from 'vue'
import type { VisualizationIdentifier } from 'ydoc-shared/yjsModel'

export interface VisualizationConfig {
  background?: string
  /** Possible visualization types that can be switched to. */
  readonly types: Iterable<VisualizationIdentifier>
  readonly currentType: VisualizationIdentifier
  readonly icon: Icon | URLString | undefined
  readonly isCircularMenuVisible: boolean
  readonly nodeSize: Vec2
  readonly scale: number
  readonly isFocused: boolean
  readonly nodeType: string | undefined
  readonly isPreview: boolean
  readonly isFullscreenAllowed: boolean
  readonly isResizable: boolean
  isBelowToolbar: boolean
  width: number
  height: number
  nodePosition: Vec2
  fullscreen: boolean
  savedSize: SavedSize | undefined
  hide: () => void
  updateType: (type: VisualizationIdentifier) => void
  createNodes: (...options: NodeCreationOptions[]) => void
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
