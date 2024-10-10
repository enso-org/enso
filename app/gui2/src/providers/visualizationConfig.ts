import { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import { ToolbarItem } from '@/components/visualizations/toolbar'
import { createContextStore } from '@/providers'
import { Vec2 } from '@/util/data/vec2'
import { ToValue } from '@/util/reactivity'
import { reactive } from 'vue'

export interface VisualizationConfig {
  /** The Enso type of the data being visualized. */
  readonly nodeType: string | undefined
  /** The size of the area available for the visualization to draw its content. */
  readonly size: Vec2
  /** Create graph nodes. */
  createNodes: (...options: NodeCreationOptions[]) => void
  /** Set the preprocessor that prepares the visualization data on the backend. */
  setPreprocessor: (
    visualizationModule: string,
    expression: string,
    ...positionalArgumentsExpressions: string[]
  ) => void
  /** Provide a toolbar definition. */
  setToolbar: (toolbar: ToValue<Readonly<ToolbarItem[]>>) => void
  /**
   * If set to `true`, the toolbar will be overlayed on top of the visualization, instead of in a space reserved above
   * it. By default, this is `false`.
   */
  setToolbarOverlay: (enableOverlay: boolean) => void
}

export { provideFn as provideVisualizationConfig }
const { provideFn, injectFn } = createContextStore(
  'Visualization config',
  reactive<VisualizationConfig>,
)

// The visualization config public API should not expose the `allowMissing` parameter. It should
// look like an ordinary vue composable.

/** TODO: Add docs */
export function useVisualizationConfig() {
  return injectFn()
}
