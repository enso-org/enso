import * as vue from 'vue'
import { type DefineComponent } from 'vue'

import { defineStore } from 'pinia'

import VisualizationContainer from '@/components/VisualizationContainer.vue'
import * as useVisualizationConfig from '@/providers/useVisualizationConfig'
import Compiler from '@/workers/visualizationCompiler?worker'
import * as vueUseCore from '@vueuse/core'
import type { VisualizationConfiguration } from 'shared/languageServerTypes'

/** A module containing the default visualization function. */
const DEFAULT_VISUALIZATION_MODULE = 'Standard.Visualization.Preprocessor'
/** A name of the default visualization function. */
const DEFAULT_VISUALIZATION_FUNCTION = 'default_preprocessor'
/** A list of arguments passed to the default visualization function. */
const DEFAULT_VISUALIZATION_ARGUMENTS: string[] = []

export const DEFAULT_VISUALIZATION_CONFIGURATION = {
  visualizationModule: DEFAULT_VISUALIZATION_MODULE,
  expression: DEFAULT_VISUALIZATION_FUNCTION,
  positionalArgumentsExpressions: DEFAULT_VISUALIZATION_ARGUMENTS,
} satisfies Partial<VisualizationConfiguration>

const moduleCache: Record<string, any> = {
  vue,
  '@vueuse/core': vueUseCore,
  'builtins/VisualizationContainer.vue': { default: VisualizationContainer },
  'builtins/useVisualizationConfig.ts': useVisualizationConfig,
}
// @ts-expect-error Intentionally not defined in `env.d.ts` as it is a mistake to access anywhere
// else.
window.__visualizationModules = moduleCache

export type Visualization = DefineComponent<
  // Props
  { data: {} },
  {},
  {},
  {},
  {},
  {},
  {},
  // Emits
  {
    'update:preprocessor': (module: string, method: string, ...args: string[]) => void
  }
>
type VisualizationModule = {
  default: Visualization
  name: string
  inputType: string
  scripts?: string[]
  styles?: string[]
}

const builtinVisualizationImports: Record<string, () => Promise<VisualizationModule>> = {
  JSON: () => import('@/components/visualizations/JSONVisualization.vue') as any,
  Table: () => import('@/components/visualizations/TableVisualization.vue') as any,
  Scatterplot: () => import('@/components/visualizations/ScatterplotVisualization.vue') as any,
  Histogram: () => import('@/components/visualizations/HistogramVisualization.vue') as any,
  Heatmap: () => import('@/components/visualizations/HeatmapVisualization.vue') as any,
  'SQL Query': () => import('@/components/visualizations/SQLVisualization.vue') as any,
  'Geo Map': () => import('@/components/visualizations/GeoMapVisualization.vue') as any,
  Image: () => import('@/components/visualizations/ImageBase64Visualization.vue') as any,
  Warnings: () => import('@/components/visualizations/WarningsVisualization.vue') as any,
}

const dynamicVisualizationPaths: Record<string, string> = {
  'Scatterplot 2': '/visualizations/ScatterplotVisualization.vue',
  'Geo Map 2': '/visualizations/GeoMapVisualization.vue',
}

export const useVisualizationStore = defineStore('visualization', () => {
  // TODO [sb]: Figure out how to list visualizations defined by a project.
  const imports = { ...builtinVisualizationImports }
  const paths = { ...dynamicVisualizationPaths }
  let cache: Record<string, VisualizationModule> = {}
  const types = [...Object.keys(imports), ...Object.keys(paths)]
  let worker: Worker | undefined
  let workerMessageId = 0
  const workerCallbacks: Record<
    string,
    { resolve: (result: VisualizationModule) => void; reject: () => void }
  > = {}

  function register(module: VisualizationModule) {
    console.log(`registering visualization: name=${module.name}, inputType=${module.inputType}`)
  }

  async function compile(path: string) {
    if (worker == null) {
      worker = new Compiler()
      worker.postMessage({ type: 'register-builtin-modules', modules: Object.keys(moduleCache) })
      worker.addEventListener(
        'message',
        async (
          event: MessageEvent<
            | { type: 'style'; code: string }
            | { type: 'raw-import'; path: string; value: unknown }
            | { type: 'url-import'; path: string; mimeType: string; value: BlobPart }
            | { type: 'import'; path: string; code: string }
            | { type: 'compilation-result'; id: number; path: string }
          >,
        ) => {
          switch (event.data.type) {
            case 'style': {
              const styleNode = document.createElement('style')
              styleNode.innerHTML = event.data.code
              document.head.appendChild(styleNode)
              break
            }
            case 'raw-import': {
              moduleCache[event.data.path] = event.data.value
              break
            }
            case 'url-import': {
              moduleCache[event.data.path] = {
                default: URL.createObjectURL(
                  new Blob([event.data.value], { type: event.data.mimeType }),
                ),
              }
              break
            }
            case 'import': {
              const module = import(
                /* @vite-ignore */
                URL.createObjectURL(new Blob([event.data.code], { type: 'text/javascript' }))
              )
              moduleCache[event.data.path] = module
              moduleCache[event.data.path] = await module
              break
            }
            case 'compilation-result': {
              workerCallbacks[event.data.id]?.resolve(moduleCache[event.data.path])
              break
            }
          }
        },
      )
      worker.addEventListener('error', (event) => {
        if (event.error?.id) {
          workerCallbacks[event.error.id]?.reject()
        }
      })
    }
    const id = workerMessageId
    workerMessageId += 1
    const promise = new Promise<VisualizationModule>((resolve, reject) => {
      workerCallbacks[id] = { resolve, reject }
    })
    worker.postMessage({ type: 'compile', id, path })
    return await promise
  }

  const scriptsNode = document.head.appendChild(document.createElement('div'))
  scriptsNode.classList.add('visualization-scripts')
  const loadedScripts = new Set<string>()
  function loadScripts(module: VisualizationModule) {
    const promises: Promise<void>[] = []
    if ('scripts' in module && module.scripts) {
      if (!Array.isArray(module.scripts)) {
        console.warn('Visualiation scripts should be an array:', module.scripts)
      }
      const scripts = Array.isArray(module.scripts) ? module.scripts : [module.scripts]
      for (const url of scripts) {
        if (typeof url !== 'string') {
          console.warn('Visualization script should be a string, skipping URL:', url)
        } else if (!loadedScripts.has(url)) {
          loadedScripts.add(url)
          const node = document.createElement('script')
          node.src = url
          promises.push(
            new Promise<void>((resolve, reject) => {
              node.addEventListener('load', () => {
                resolve()
              })
              node.addEventListener('error', () => {
                reject()
              })
            }),
          )
          scriptsNode.appendChild(node)
        }
      }
    }
    return Promise.allSettled(promises)
  }

  // NOTE: Because visualization scripts are cached, they are not guaranteed to be up to date.
  async function get(type: string) {
    let module = cache[type]
    if (module == null) {
      module = await imports[type]?.()
    }
    if (module == null) {
      const path = paths[type]
      if (path != null) {
        module = await compile(path)
      }
    }
    if (module == null) {
      return
    }
    register(module)
    await loadScripts(module)
    cache[type] = module
    return module.default
  }

  function clear() {
    cache = {}
  }

  return { types, get, clear }
})
