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

  function sampleData(type: string) {
    switch (type) {
      case 'Warnings': {
        return ['warning 1', "warning 2!!&<>;'\x22"]
      }
      case 'Bubble': {
        return [
          [10, 10, 10],
          [10, 100, 10],
          [100, 10, 10],
          [100, 100, 10],
          [25, 25, 5],
          [25, 85, 5],
          [85, 25, 5],
          [85, 85, 5],
          [55, 55, 20],
        ]
      }
      case 'Image': {
        return {
          mediaType: 'image/svg+xml',
          base64: `PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI0MCIgaGVpZ2h0PSI0\
MCI+PGcgY2xpcC1wYXRoPSJ1cmwoI2EpIj48cGF0aCBkPSJNMjAuMDUgMEEyMCAyMCAwIDAgMCAwIDIwLjA1IDIwLjA2IDIwLjA\
2IDAgMSAwIDIwLjA1IDBabTAgMzYuMDVjLTguOTMgMC0xNi4xLTcuMTctMTYuMS0xNi4xIDAtOC45NCA3LjE3LTE2LjEgMTYuMS\
0xNi4xIDguOTQgMCAxNi4xIDcuMTYgMTYuMSAxNi4xYTE2LjE4IDE2LjE4IDAgMCAxLTE2LjEgMTYuMVoiLz48cGF0aCBkPSJNM\
jcuMTIgMTcuNzdhNC42OCA0LjY4IDAgMCAxIDIuMzkgNS45MiAxMC4yMiAxMC4yMiAwIDAgMS05LjU2IDYuODZBMTAuMiAxMC4y\
IDAgMCAxIDkuNzcgMjAuMzZzMS41NSAyLjA4IDQuNTcgMi4wOGMzLjAxIDAgNC4zNi0xLjE0IDUuNi0yLjA4IDEuMjUtLjkzIDI\
uMDktMyA1LjItMyAuNzMgMCAxLjQ2LjIgMS45OC40WiIvPjwvZz48ZGVmcz48Y2xpcFBhdGggaWQ9ImEiPjxwYXRoIGZpbGw9Ii\
NmZmYiIGQ9Ik0wIDBoNDB2NDBIMHoiLz48L2NsaXBQYXRoPjwvZGVmcz48L3N2Zz4=`,
        }
      }
      default: {
        return {}
      }
    }
  }

  return { types, get, sampleData, clear }
})
