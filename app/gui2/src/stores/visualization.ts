import * as vue from 'vue'
import { computed, reactive, type DefineComponent, type PropType } from 'vue'

import VisualizationContainer from '@/components/VisualizationContainer.vue'
import { useVisualizationConfig } from '@/providers/visualizationConfig'
import { useProjectStore } from '@/stores/project'
import { assertNever } from '@/util/assert'
import { rpcWithRetries } from '@/util/net'
import type { Opt } from '@/util/opt'
import { defineKeybinds } from '@/util/shortcuts'
import builtinVisualizationMetadata from '@/util/visualizationMetadata.json'
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
import type { LanguageServer } from 'shared/languageServer'
import type { FileEventKind, Path, VisualizationConfiguration } from 'shared/languageServerTypes'
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
  'Scatter Plot': '/visualizations/ScatterplotVisualization.vue',
  'Geo Map': '/visualizations/GeoMapVisualization.vue',
}

declare const visualizationCacheKeyBrand: unique symbol
type VisualizationCacheKey = string & { [visualizationCacheKeyBrand]: never }

export const useVisualizationStore = defineStore('visualization', () => {
  const imports = { ...builtinVisualizationImports }
  const paths = { ...dynamicVisualizationPaths }
  const cache = reactive(new Map<VisualizationCacheKey, Promise<VisualizationModule>>())
  const compilationAbortControllers = reactive(
    new Map<string /* path, excluding leading / */, AbortController>(),
  )
  const currentProjectVisualizationsByPath = new Map<string, string>()
  let worker: Worker | undefined
  let workerMessageId = 0
  const workerCallbacks = new Map<
    number,
    { resolve: (result: VisualizationModule) => void; reject: (error: Error) => void }
  >()
  const allBuiltinVisualizations = [
    ...Object.keys(imports),
    ...Object.keys(paths),
  ].map<VisualizationIdentifier>((name) => ({
    module: { kind: 'Builtin' },
    name,
  }))
  const visualizationsForType = reactive(new Map<string, Set<VisualizationCacheKey>>())
  const typesForVisualization = reactive(new Map<string, ReadonlySet<string>>())
  const proj = useProjectStore()
  const ls = proj.lsRpcConnection
  const data = proj.dataConnection
  const projectRoot = proj.contentRoots.then(
    (roots) => roots.find((root) => root.type === 'Project') ?? null,
  )

  function getTypesFromUnion(inputType: Opt<string>) {
    return new Set(inputType?.split('|').map((type) => type.trim()) ?? ['Any'])
  }

  function removeVisualizationTypes(id: VisualizationIdentifier, name: string) {
    const key = toVisualizationCacheKey(id)
    const types = typesForVisualization.get(name)
    if (!types) return
    typesForVisualization.delete(name)
    for (const type of types) {
      visualizationsForType.get(type)?.delete(key)
    }
  }

  function updateVisualizationTypes(
    id: VisualizationIdentifier,
    name: string,
    inputType: Opt<string>,
  ) {
    const key = toVisualizationCacheKey(id)
    const newTypes = getTypesFromUnion(inputType)
    const types = typesForVisualization.get(name)
    typesForVisualization.set(name, newTypes)
    if (types) {
      for (const type of types) {
        if (!newTypes.has(type)) {
          visualizationsForType.get(type)?.delete(key)
        }
      }
    }
    for (const type of newTypes) {
      if (!types || !types.has(type)) {
        let set = visualizationsForType.get(type)
        if (!set) {
          set = new Set()
          visualizationsForType.set(type, set)
        }
        set.add(key)
      }
    }
  }

  for (const { name, inputType } of builtinVisualizationMetadata) {
    if (name === 'Loading') continue
    updateVisualizationTypes(
      {
        module: { kind: 'Builtin' },
        name,
      },
      name,
      inputType,
    )
  }

  function types(type: string | undefined) {
    const ret =
      type === undefined
        ? allBuiltinVisualizations
        : [
            ...(visualizationsForType.get(type) ?? []),
            ...(visualizationsForType.get('Any') ?? []),
          ].map(fromVisualizationCacheKey)
    return ret
  }

  function postMessage<T>(worker: Worker, message: T) {
    worker.postMessage(message)
  }

  // TODO: remove outdated styles
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
              const module = await moduleCache[event.data.path]
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
                const url = new URL(event.data.path, location.href)
                switch (url.protocol) {
                  case 'http:':
                  case 'https:': {
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
                  case 'enso-current-project:': {
                    const rootId = (await projectRoot)?.id
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
                    const payload = await (
                      await data
                    ).readFile({ rootId, segments: url.pathname.split('/') })
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
      worker.addEventListener('error', (event) => console.error(event.error))
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

  function fromVisualizationCacheKey(key: VisualizationCacheKey): VisualizationIdentifier {
    return JSON.parse(key)
  }

  async function onFileEvent(event: { path: Path; kind: FileEventKind }) {
    const pathString = event.path.segments.join('/')
    const name = currentProjectVisualizationsByPath.get(pathString)
    const id: VisualizationIdentifier | undefined =
      name != null ? { module: { kind: 'CurrentProject' }, name } : undefined
    const key = id && toVisualizationCacheKey(id)
    if (event.path.segments[0] === 'visualizations' && /\.vue$/.test(pathString)) {
      compilationAbortControllers.get(pathString)?.abort()
      compilationAbortControllers.delete(pathString)
      switch (event.kind) {
        case 'Added': {
          try {
            const abortController = new AbortController()
            compilationAbortControllers.set(pathString, abortController)
            const vizPromise = compile('enso-current-project:' + pathString)
            const viz = await vizPromise
            if (abortController.signal.aborted) break
            const id: VisualizationIdentifier = {
              module: { kind: 'CurrentProject' },
              name: viz.name,
            }
            const key = toVisualizationCacheKey(id)
            updateVisualizationTypes(id, viz.name, viz.inputType)
            cache.set(key, vizPromise)
          } catch {
            // Ignored - the file is not a module.
          }
          break
        }
        case 'Modified': {
          if (!key) break
          try {
            const abortController = new AbortController()
            compilationAbortControllers.set(pathString, abortController)
            const vizPromise = compile('enso-current-project:' + pathString)
            cache.set(key, compile('enso-current-project:' + pathString))
            const viz = await vizPromise
            if (abortController.signal.aborted) break
            if (viz.name !== id.name) {
              removeVisualizationTypes(id, id.name)
              updateVisualizationTypes(
                { module: { kind: 'CurrentProject' }, name: viz.name },
                viz.name,
                viz.inputType,
              )
            } else {
              updateVisualizationTypes(id, viz.name, viz.inputType)
            }
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
  }

  async function walkFiles(ls: LanguageServer, path: Path, cb: (path: Path) => void) {
    for (const file of (await ls.listFiles(path)).paths) {
      const filePath: Path = {
        rootId: file.path.rootId,
        segments: [...file.path.segments, file.name],
      }
      switch (file.type) {
        case 'Directory':
        case 'DirectoryTruncated': {
          await walkFiles(ls, filePath, cb)
          break
        }
        case 'File': {
          cb(filePath)
          break
        }
        case 'Other':
        case 'SymlinkLoop': {
          // Ignored.
          break
        }
        default: {
          assertNever(file)
        }
      }
    }
  }

  ls.then(async (ls) => {
    const projectRoot_ = await projectRoot
    if (!projectRoot_) {
      console.error('Could not load custom visualizations: File system content root not found.')
      return
    }
    await rpcWithRetries(() =>
      ls.acquireCapability('file/receivesTreeUpdates', {
        path: { rootId: projectRoot_.id, segments: [] } satisfies Path,
      }),
    )
    ls.on('file/event', onFileEvent)
    await walkFiles(ls, { rootId: projectRoot_.id, segments: ['visualizations'] }, (path) =>
      onFileEvent({
        kind: 'Added',
        path,
      }),
    )
  })

  async function get(meta: VisualizationIdentifier, ignoreCache = false) {
    const key = toVisualizationCacheKey(meta)
    if (!cache.get(key) || ignoreCache) {
      switch (meta.module.kind) {
        case 'Builtin': {
          cache.set(key, resolveBuiltinVisualization(meta.name))
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
    }
    return computed(() => cache.get(key))
  }

  async function resolveBuiltinVisualization(type: string) {
    const builtinImport = imports[type]?.()
    if (builtinImport) {
      const module = await builtinImport
      await loadScripts(module)
      return module
    }
    const builtinDynamicPath = paths[type]
    if (builtinDynamicPath != null) {
      const module = await compile(builtinDynamicPath)
      await loadScripts(module)
      return module
    }
    throw new Error(`Unknown visualization type: ${type}`)
  }

  return { types, get }
})
