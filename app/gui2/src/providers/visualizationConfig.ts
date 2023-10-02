import { Vec2 } from '@/util/vec2'
import { inject, provide, type InjectionKey, type Ref } from 'vue'

export interface VisualizationConfig {
  /** Possible visualization types that can be switched to. */
  background?: string
  readonly types: string[]
  readonly isCircularMenuVisible: boolean
  readonly nodeSize: Vec2
  width: number | null
  height: number | null
  fullscreen: boolean
  hide: () => void
  updateType: (type: string) => void
}

export function defaultVisualizationConfig(): VisualizationConfig {
  return {
    fullscreen: false,
    width: 200,
    height: 150,
    hide() {},
    isCircularMenuVisible: false,
    nodeSize: new Vec2(200, 150),
    types: ['Example', 'Types', 'Here'],
    updateType() {},
  }
}

export const visualizationConfigProvideKey$FOR$INTERNAL$USE$ONLY = Symbol(
  'visualizationConfig',
) as InjectionKey<Ref<VisualizationConfig>>

export function useVisualizationConfig(): Ref<VisualizationConfig> {
  const injected = inject(visualizationConfigProvideKey$FOR$INTERNAL$USE$ONLY)
  if (injected == null) throw new Error('Visualization config not provided')
  return injected
}

export function provideVisualizationConfig(visualizationConfig: Ref<VisualizationConfig>) {
  provide(visualizationConfigProvideKey$FOR$INTERNAL$USE$ONLY, visualizationConfig)
}
