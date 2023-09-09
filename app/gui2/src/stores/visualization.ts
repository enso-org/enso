import Compiler from '@/workers/visualizationCompiler?worker'
import * as vue from 'vue'

import { defineStore } from 'pinia'

// @ts-expect-error
window.vue = vue

type VisualizationModule =
  typeof import('../../public/visualizations/VisualizationContainer.vue') & {
    name?: string
    inputType?: string
    scripts?: string[]
    styles?: string[]
  }

export type Visualization = VisualizationModule['default']

export const useVisualizationStore = defineStore('visualization', () => {
  // FIXME: statically resolved imports will not work for user-defined components
  const paths: Record<string, string> = {
    JSON: '/visualizations/JSONVisualization.vue',
    Error: '/visualizations/ErrorVisualization.vue',
    Warnings: '/visualizations/WarningsVisualization.vue',
    Bubble: '/visualizations/BubbleVisualization.vue',
    Image: '/visualizations/ImageBase64Visualization.vue',
    'Geo Map': '/visualizations/GeoMapVisualization.vue',
    Scatterplot: '/visualizations/ScatterplotVisualization.vue',
  } as any
  let cache: Record<string, any> = {}
  const types = Object.keys(paths)
  let worker: Worker | undefined
  let workerMessageId = 0
  const workerCallbacks: Record<
    string,
    {
      resolve: (result: string) => void
      reject: () => void
    }
  > = {}

  function register(name: string, inputType: string) {
    console.log(`registering visualization: name=${name}, inputType=${inputType}`)
  }

  async function compile(path: string) {
    if (worker == null) {
      worker = new Compiler()
      worker.addEventListener(
        'message',
        (
          event: MessageEvent<
            { type: 'style'; code: string } | { type: 'script'; id: number; code: string }
          >,
        ) => {
          switch (event.data.type) {
            case 'style': {
              const styleNode = document.createElement('style')
              styleNode.innerHTML = event.data.code
              document.head.appendChild(styleNode)
              break
            }
            case 'script': {
              workerCallbacks[event.data.id].resolve(event.data.code)
              break
            }
          }
        },
      )
      worker.addEventListener('error', (event) => {
        workerCallbacks[event.error.id].reject()
      })
    }
    const id = workerMessageId
    workerMessageId += 1
    const promise = new Promise<string>((resolve, reject) => {
      workerCallbacks[id] = { resolve, reject }
    })
    worker.postMessage({ id, path })
    return await promise
  }

  // NOTE: Because visualization scripts are cached, they are not guaranteed to be up to date.
  async function get(type: string) {
    let component: Visualization = cache[type]
    if (component == null) {
      const script = await compile(paths[type])
      const module = await import(
        /* @vite-ignore */ `data:text/javascript,${encodeURIComponent(script)}`
      )
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
      case 'Error': {
        return { kind: 'Dataflow', message: 'a data flow error' }
      }
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
      case 'JSON':
      case 'Scatterplot': {
        return {
          axis: {
            x: { label: 'x-axis label', scale: 'linear' },
            y: { label: 'y-axis label', scale: 'logarithmic' },
          },
          focus: { x: 1.7, y: 2.1, zoom: 3.0 },
          points: { labels: 'visible' },
          data: [
            { x: 0.1, y: 0.7, label: 'foo', color: 'FF0000', shape: 'circle', size: 0.2 },
            { x: 0.4, y: 0.2, label: 'baz', color: '0000FF', shape: 'square', size: 0.3 },
          ],
        }
      }
      case 'Geo Map': {
        return {
          latitude: 37.8,
          longitude: -122.45,
          zoom: 15,
          controller: true,
          showingLabels: true, // Enables presenting labels when hovering over a point.
          layers: [
            {
              type: 'Scatterplot_Layer',
              data: [
                {
                  latitude: 37.8,
                  longitude: -122.45,
                  color: [255, 0, 0],
                  radius: 100,
                  label: 'an example label',
                },
              ],
            },
          ],
        }
      }
      default: {
        return {}
      }
    }
  }

  return { types, get, sampleData, clear }
})
