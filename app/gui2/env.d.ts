/// <reference types="vite/client" />

declare const PROJECT_MANAGER_URL: string
declare const HISTOIRE: boolean | undefined
declare const RUNNING_VITEST: boolean

declare module 'builtins' {
  export const VisualizationContainer: typeof import('@/components/VisualizationContainer.vue').default
  export const useVisualizationConfig: typeof import('@/providers/visualizationConfig').useVisualizationConfig
  export const defineKeybinds: typeof import('@/util/shortcuts').defineKeybinds
}
