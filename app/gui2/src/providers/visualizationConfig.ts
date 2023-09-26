import type { Vec2 } from '@/util/vec2'
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

const provideKey = Symbol('visualizationConfig') as InjectionKey<Ref<VisualizationConfig>>

export function useVisualizationConfig(): Ref<VisualizationConfig> {
  const injected = inject(provideKey)
  if (injected == null) throw new Error('AppConfig not provided')
  return injected
}

export function provideVisualizationConfig(visualizationConfig: Ref<VisualizationConfig>) {
  provide(provideKey, visualizationConfig)
}
