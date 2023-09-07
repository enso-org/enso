import { defineStore } from 'pinia'

export type VisualizationModule = typeof import('visualizations/VisualizationContainer.vue') & {
  name?: string
  inputType?: string
}

export type Visualization = VisualizationModule['default']

export const useVisualizationStore = defineStore('visualization', () => {
  // FIXME: statically resolved imports will not work for user-defined components
  const getters: Record<string, () => Promise<VisualizationModule>> = {
    Warnings: () => import('visualizations/WarningsVisualization.vue'),
    Bubble: () => import('visualizations/BubbleVisualization.vue'),
    Image: () => import('visualizations/ImageBase64Visualization.vue'),
    GeoMap: () => import('visualizations/GeoMapVisualization.vue'),
    Scatterplot: () => import('visualizations/ScatterplotVisualization.vue'),
  } as any
  let cache: Record<string, any> = {}
  const types = Object.keys(getters)

  function register(name: string, inputType: string) {
    console.log(`registering visualization: name=${name}, inputType=${inputType}`)
  }

  // NOTE: Because visualization scripts are cached, they are not guaranteed to be up to date.
  async function get(type: string) {
    let component: VisualizationModule['default'] = cache[type]
    if (component == null) {
      const module = await getters[type]()
      // TODO[sb]: fallback to name based on path to visualization.
      register(module.name ?? type, module.inputType ?? 'Any')
      component = module.default
      cache[type] = component
    }
    return component
  }

  function clear() {
    cache = {}
  }

  return { types, get, clear }
})
