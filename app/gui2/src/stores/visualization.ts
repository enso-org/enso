import * as vue from 'vue'
import { type DefineComponent } from 'vue'

import * as vueUseCore from '@vueuse/core'
import { defineStore } from 'pinia'

import VisualizationContainer from '@/components/VisualizationContainer.vue'
import * as useVisualizationConfig from '@/providers/useVisualizationConfig'
import type {
  AddImportNotification,
  AddRawImportNotification,
  AddStyleNotification,
  AddURLImportNotification,
  CompilationErrorResponse,
  CompilationResultResponse,
  CompileError,
  CompileRequest,
  FetchError,
  InvalidMimetypeError,
  RegisterBuiltinModulesRequest,
} from '@/workers/visualizationCompiler'
import Compiler from '@/workers/visualizationCompiler?worker'
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
  Histogram: () => import('@/components/visualizations/HistogramVisualization.vue') as any,
  Heatmap: () => import('@/components/visualizations/HeatmapVisualization.vue') as any,
  'SQL Query': () => import('@/components/visualizations/SQLVisualization.vue') as any,
  Image: () => import('@/components/visualizations/ImageBase64Visualization.vue') as any,
  Warnings: () => import('@/components/visualizations/WarningsVisualization.vue') as any,
}

const dynamicVisualizationPaths: Record<string, string> = {
  Scatterplot: '/visualizations/ScatterplotVisualization.vue',
  'Geo Map': '/visualizations/GeoMapVisualization.vue',
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

  function postMessage<T>(worker: Worker, message: T) {
    worker.postMessage(message)
  }

  async function compile(path: string) {
    if (worker == null) {
      worker = new Compiler()
      postMessage<RegisterBuiltinModulesRequest>(worker, {
        type: 'register-builtin-modules-request',
        modules: Object.keys(moduleCache),
      })
      worker.addEventListener(
        'message',
        async (
          event: MessageEvent<
            // === Responses ===
            | CompilationResultResponse
            | CompilationErrorResponse
            // === Notifications ===
            | AddStyleNotification
            | AddRawImportNotification
            | AddURLImportNotification
            | AddImportNotification
            // === Errors ===
            | FetchError
            | InvalidMimetypeError
            | CompileError
          >,
        ) => {
          switch (event.data.type) {
            // === Responses ===
            case 'compilation-result-response': {
              workerCallbacks[event.data.id]?.resolve(moduleCache[event.data.path])
              break
            }
            case 'compilation-error-response': {
              console.error(`Error compiling visualization '${event.data.path}':`, event.data.error)
              workerCallbacks[event.data.id]?.reject()
              break
            }
            // === Notifications ===
            case 'add-style-notification': {
              const styleNode = document.createElement('style')
              styleNode.innerHTML = event.data.code
              document.head.appendChild(styleNode)
              break
            }
            case 'add-raw-import-notification': {
              moduleCache[event.data.path] = event.data.value
              break
            }
            case 'add-url-import-notification': {
              moduleCache[event.data.path] = {
                default: URL.createObjectURL(
                  new Blob([event.data.value], { type: event.data.mimeType }),
                ),
              }
              break
            }
            case 'add-import-notification': {
              const module = import(
                /* @vite-ignore */
                URL.createObjectURL(new Blob([event.data.code], { type: 'text/javascript' }))
              )
              moduleCache[event.data.path] = module
              moduleCache[event.data.path] = await module
              break
            }
            // === Errors ===
            case 'fetch-error': {
              console.error(`Error fetching '${event.data.path}':`, event.data.error)
              break
            }
            case 'invalid-mimetype-error': {
              console.error(
                `Expected mimetype of '${event.data.path}' to be '${event.data.expected}', ` +
                  `but received '${event.data.actual}' instead`,
              )
              break
            }
            case 'compile-error': {
              console.error(`Error compiling '${event.data.path}':`, event.data.error)
              break
            }
          }
        },
      )
      worker.addEventListener('error', (event) => {
        console.error(event.error)
      })
    }
    const id = workerMessageId
    workerMessageId += 1
    const promise = new Promise<VisualizationModule>((resolve, reject) => {
      workerCallbacks[id] = { resolve, reject }
    })
    postMessage<CompileRequest>(worker, { type: 'compile-request', id, path })
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
