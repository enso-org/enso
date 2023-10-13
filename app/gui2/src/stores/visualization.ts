import * as vue from 'vue'
import { reactive, ref, type DefineComponent, type PropType } from 'vue'

import VisualizationContainer from '@/components/VisualizationContainer.vue'
import { useVisualizationConfig } from '@/providers/visualizationConfig'
import { defineKeybinds } from '@/util/shortcuts'
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
import { defineStore } from 'pinia'
import type { VisualizationConfiguration } from 'shared/languageServerTypes'
import type { VisualizationIdentifier } from 'shared/yjsModel'

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

export const DEFAULT_VISUALIZATION_IDENTIFIER: VisualizationIdentifier = {
  module: { kind: 'Builtin' },
  name: 'JSON',
}

const moduleCache: Record<string, any> = {
  vue,
  get d3() {
    return import('d3')
  },
  builtins: { VisualizationContainer, useVisualizationConfig, defineKeybinds },
}
// @ts-expect-error Intentionally not defined in `env.d.ts` as it is a mistake to access anywhere
// else.
window.__visualizationModules = moduleCache

export type Visualization = DefineComponent<
  // Props
  { data: { type: PropType<unknown>; required: true } },
  {},
  unknown,
  {},
  {},
  {},
  {},
  // Emits
  {
    'update:preprocessor'?: (module: string, method: string, ...args: string[]) => void
  }
>
type VisualizationModule = {
  default: Visualization
  name: string
  inputType?: string
  defaultPreprocessor?: readonly [module: string, method: string, ...args: string[]]
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
  const imports = { ...builtinVisualizationImports }
  const paths = { ...dynamicVisualizationPaths }
  let cache: Record<string, VisualizationModule> = {}
  let worker: Worker | undefined
  let workerMessageId = 0
  const workerCallbacks: Record<
    string,
    { resolve: (result: VisualizationModule) => void; reject: () => void }
  > = {}
  const allVisualizations = [
    ...Object.keys(imports),
    ...Object.keys(paths),
  ].map<VisualizationIdentifier>((name) => ({
    module: { kind: 'Builtin' },
    name,
  }))
  const visualizationsForType = reactive(new Map<string, readonly VisualizationIdentifier[]>())
  const visualizationsForAny = ref<readonly VisualizationIdentifier[]>([])

  Promise.all([
    ...Object.values(builtinVisualizationImports).map((importer) => importer()),
    ...Object.values(dynamicVisualizationPaths).map(compile),
  ])
    .then((modules) =>
      Object.fromEntries(
        modules.map((module) => [
          module.name,
          new Set(
            module.inputType == null
              ? ['Any']
              : module.inputType.split('|').map((type) => type.trim()),
          ),
        ]),
      ),
    )
    .then((moduleInputTypes) => {
      const types = Object.values(moduleInputTypes).flatMap((set) => Array.from(set))
      for (const type of types) {
        if (visualizationsForType.has(type)) {
          continue
        }
        const matchingTypes = Object.entries(moduleInputTypes).flatMap<VisualizationIdentifier>(
          ([name, inputTypes]) =>
            inputTypes.has(type) || inputTypes.has('Any')
              ? [
                  {
                    module: { kind: 'Builtin' },
                    name,
                  },
                ]
              : [],
        )
        if (type === 'Any') {
          visualizationsForAny.value = matchingTypes
        }
        visualizationsForType.set(type, matchingTypes)
      }
    })

  function types(type: string | undefined) {
    const ret =
      type === undefined
        ? allVisualizations
        : visualizationsForType.get(type) ?? visualizationsForAny.value
    return ret
  }

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
  async function get(meta: VisualizationIdentifier) {
    if (meta.module.kind !== 'Builtin') {
      console.warn('Custom visualization module support is not yet implemented:', meta.module)
      return
    }
    const type = meta.name
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
    return module
  }

  function clear() {
    cache = {}
  }

  return { types, get, clear }
})
