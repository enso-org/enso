/// <reference types="vite/client" />

declare const PROJECT_MANAGER_URL: string
declare const RUNNING_VITEST: boolean

// This is an augmentation to the built-in `ImportMeta` interface.
// This file MUST NOT contain any top-level imports.
// interface ImportMeta {
//   vitest: typeof import('vitest') | undefined
// }

declare module 'builtins' {
  export const VisualizationContainer: typeof import('@/components/VisualizationContainer.vue').default
  export const useVisualizationConfig: typeof import('@/providers/visualizationConfig').useVisualizationConfig
  export const defineKeybinds: typeof import('@/util/shortcuts').defineKeybinds
}
