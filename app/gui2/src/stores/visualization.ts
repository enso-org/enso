import * as vue from 'vue'
import { computed, reactive, ref, type DefineComponent, type PropType } from 'vue'

import VisualizationContainer from '@/components/VisualizationContainer.vue'
import { useVisualizationConfig } from '@/providers/visualizationConfig'
import { useProjectStore } from '@/stores/project'
import { assertNever } from '@/util/assert'
import { rpcWithRetries } from '@/util/net'
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
  FetchResultWorkerResponse,
  FetchWorkerError,
  FetchWorkerRequest,
  InvalidMimetypeError,
  RegisterBuiltinModulesRequest,
} from '@/workers/visualizationCompiler'
import Compiler from '@/workers/visualizationCompiler?worker'
import { defineStore } from 'pinia'
import type { Path, VisualizationConfiguration } from 'shared/languageServerTypes'
import type { VisualizationIdentifier } from 'shared/yjsModel'
import { z } from 'zod'

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

const VisualizationModule = z.object({
  default: z.unknown(),
  name: z.string(),
  inputType: z.string().optional(),
  defaultPreprocessor: (
    z.string().array().min(2) as unknown as z.ZodType<
      [module: string, method: string, ...args: string[]]
    >
  )
    .readonly()
    .optional(),
  scripts: z.string().array().optional(),
  styles: z.string().array().optional(),
})
type VisualizationModule = z.infer<typeof VisualizationModule>

class InvalidVisualizationModuleError extends TypeError {
  constructor(public path: string) {
    super(`The module '${path}' is not a visualization.`)
  }
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

declare const visualizationCacheKeyBrand: unique symbol
type VisualizationCacheKey = string & { [visualizationCacheKeyBrand]: never }

export const useVisualizationStore = defineStore('visualization', () => {
  const imports = { ...builtinVisualizationImports }
  const paths = { ...dynamicVisualizationPaths }
  const cache = reactive(new Map<VisualizationCacheKey, VisualizationModule>())
  const currentProjectVisualizationsByPath = new Map<string, string>()
  let worker: Worker | undefined
  let workerMessageId = 0
  const workerCallbacks = new Map<
    number,
    { resolve: (result: VisualizationModule) => void; reject: (error: Error) => void }
  >()
  const allVisualizations = [
    ...Object.keys(imports),
    ...Object.keys(paths),
  ].map<VisualizationIdentifier>((name) => ({
    module: { kind: 'Builtin' },
    name,
  }))
  const visualizationsForType = reactive(new Map<string, readonly VisualizationIdentifier[]>())
  const visualizationsForAny = ref<readonly VisualizationIdentifier[]>([])
  let isTypesLookupInitialized = false
  const proj = useProjectStore()
  const ls = proj.lsRpcConnection
  const data = proj.dataConnection
  const fsRoot = proj.contentRoots.then(
    (roots) => roots.find((root) => root.type === 'FileSystemRoot') ?? null,
  )

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
      isTypesLookupInitialized = true
    })

  function types(type: string | undefined) {
    if (!isTypesLookupInitialized) return allVisualizations
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
      const worker_ = (worker = new Compiler())
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
            // === Worker Requests ===
            | FetchWorkerRequest
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
              const module = moduleCache[event.data.path]
              const vizModule = VisualizationModule.parse(module)
              if (vizModule) {
                workerCallbacks.get(event.data.id)?.resolve(vizModule)
              } else {
                workerCallbacks
                  .get(event.data.id)
                  ?.reject(new InvalidVisualizationModuleError(event.data.path))
              }
              workerCallbacks.delete(event.data.id)
              break
            }
            case 'compilation-error-response': {
              console.error(`Error compiling visualization '${event.data.path}':`, event.data.error)
              workerCallbacks.get(event.data.id)?.reject(event.data.error)
              workerCallbacks.delete(event.data.id)
              break
            }
            // === Worker Requests ===
            case 'fetch-worker-request': {
              try {
                const url = new URL(path, location.href)
                switch (url.protocol) {
                  case 'http':
                  case 'https': {
                    const response = await fetch(url)
                    if (response.ok) {
                      postMessage<FetchResultWorkerResponse>(worker_, {
                        type: 'fetch-result-worker-response',
                        path: event.data.path,
                        contents: await response.arrayBuffer(),
                        contentType: response.headers.get('Content-Type') ?? undefined,
                      })
                    } else {
                      postMessage<FetchWorkerError>(worker_, {
                        type: 'fetch-worker-error',
                        path: event.data.path,
                        error: new Error(
                          `\`fetch\` returned status code ${response.status} (${response.statusText})`,
                        ),
                      })
                    }
                    break
                  }
                  case 'enso-current-project': {
                    const rootId = (await fsRoot)?.id
                    if (!rootId) {
                      postMessage<FetchWorkerError>(worker_, {
                        type: 'fetch-worker-error',
                        path: event.data.path,
                        error: new Error(
                          'Could not find a file system content root for the current project.',
                        ),
                      })
                      break
                    }
                    const payload = await (await data).readFile(url.pathname)
                    const contents = payload.contentsArray()
                    if (!contents) {
                      postMessage<FetchWorkerError>(worker_, {
                        type: 'fetch-worker-error',
                        path: event.data.path,
                        error: new Error(
                          `An invalid response was recieved when fetching '${url.pathname}' from the current project.`,
                        ),
                      })
                      break
                    }
                    postMessage<FetchResultWorkerResponse>(worker_, {
                      type: 'fetch-result-worker-response',
                      path: event.data.path,
                      contents,
                      contentType: undefined,
                    })
                    break
                  }
                }
              } catch (error) {
                postMessage<FetchWorkerError>(worker_, {
                  type: 'fetch-worker-error',
                  path: event.data.path,
                  error: error instanceof Error ? error : new Error(`${error}`),
                })
              }
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
            default: {
              assertNever(event.data)
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
      workerCallbacks.set(id, { resolve, reject })
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

  function toVisualizationCacheKey(meta: VisualizationIdentifier) {
    return JSON.stringify({
      // All fields MUST be explicitly written so that the order is consistent.
      module: {
        kind: meta.module.kind,
        name: meta.module.kind === 'Library' ? meta.module.name : undefined,
      },
      name: meta.name,
    }) as VisualizationCacheKey
  }

  ls.then(async (ls) => {
    const fsRoot_ = await fsRoot
    if (!fsRoot_) {
      console.error('Could not load custom visualizations: File system content root not found.')
      return
    }
    await rpcWithRetries(() =>
      ls.acquireCapability('file/receivesTreeUpdates', {
        path: { rootId: fsRoot_.id, segments: [] } satisfies Path,
      }),
    )
    ls.on('file/event', async (event) => {
      console.log('file/event', event.path)
      const pathString = event.path.segments.join('/')
      const name = currentProjectVisualizationsByPath.get(pathString)
      const key =
        name != null
          ? toVisualizationCacheKey({ module: { kind: 'CurrentProject' }, name })
          : undefined
      if (event.path.segments[0] === 'visualizations') {
        switch (event.kind) {
          case 'Added': {
            const viz = await compile('enso-current-project:' + pathString)
            try {
              const key = toVisualizationCacheKey({
                module: { kind: 'CurrentProject' },
                name: viz.name,
              })
              cache.set(key, await compile('enso-current-project:' + pathString))
            } catch {
              // Ignored - the file is not a module.
            }
            break
          }
          case 'Modified': {
            if (!key) break
            try {
              cache.set(key, await compile('enso-current-project:' + pathString))
            } catch (error) {
              if (error instanceof InvalidVisualizationModuleError) {
                cache.delete(key)
              }
              // Else, ignored - the file is not a module.
            }
            break
          }
          case 'Removed': {
            currentProjectVisualizationsByPath.delete(pathString)
            if (key) cache.delete(key)
          }
        }
      }
    })
  })

  // NOTE: Because visualization scripts are cached, they are not guaranteed to be up to date.
  function get(meta: VisualizationIdentifier, ignoreCache = false) {
    const key = toVisualizationCacheKey(meta)
    switch (meta.module.kind) {
      case 'Builtin': {
        ;(async () => {
          const type = meta.name
          let module = cache.get(key)
          if (module == null) {
            module = await imports[type]?.()
          }
          if (module == null || ignoreCache) {
            const path = paths[type]
            if (path != null) {
              module = await compile(path)
            }
          }
          if (module != null) {
            register(module)
            await loadScripts(module)
            cache.set(key, module)
          }
        })()
        break
      }
      case 'CurrentProject': {
        // No special handling needed; updates are handled in an external event handler above.
        break
      }
      case 'Library': {
        console.warn('Library visualization support is not yet implemented:', meta.module)
        break
      }
    }
    return computed(() => cache.get(key))
  }

  return { types, get }
})
