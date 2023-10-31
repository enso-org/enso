import { useProjectStore } from '@/stores/project'
import {
  compile,
  InvalidVisualizationModuleError,
  type VisualizationModule,
} from '@/stores/visualization/compilerMessaging'
import builtinVisualizationMetadata from '@/stores/visualization/metadata.json'
import { assertNever } from '@/util/assert'
import { rpcWithRetries } from '@/util/net'
import type { Opt } from '@/util/opt'
import { defineStore } from 'pinia'
import type { LanguageServer } from 'shared/languageServer'
import type { FileEventKind, Path, VisualizationConfiguration } from 'shared/languageServerTypes'
import type { VisualizationIdentifier } from 'shared/yjsModel'
import { computed, reactive, type DefineComponent, type PropType } from 'vue'

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
  /** A map from file path to {@link AbortController}, so that a file change event can stop previous
   * file change event handlers for the same path. */
  const compilationAbortControllers = reactive(new Map<string, AbortController>())
  /** A map from file path in the current project, to visualization name. This is required so that
   * file delete events can remove the cached visualization. */
  const currentProjectVisualizationsByPath = new Map<string, string>()
  const allBuiltinVisualizations = [
    ...Object.keys(imports),
    ...Object.keys(paths),
  ].map<VisualizationIdentifier>((name) => ({
    module: { kind: 'Builtin' },
    name,
  }))
  const visualizationsForType = reactive(new Map<string, Set<VisualizationCacheKey>>())
  const typesForVisualization = reactive(new Map<VisualizationCacheKey, ReadonlySet<string>>())
  const proj = useProjectStore()
  const ls = proj.lsRpcConnection
  const data = proj.dataConnection
  const projectRoot = proj.contentRoots.then(
    (roots) => roots.find((root) => root.type === 'Project')?.id,
  )

  function getTypesFromUnion(inputType: Opt<string>) {
    return new Set(inputType?.split('|').map((type) => type.trim()) ?? ['Any'])
  }

  function removeVisualizationTypes(id: VisualizationIdentifier) {
    const key = toVisualizationCacheKey(id)
    const types = typesForVisualization.get(key)
    if (!types) return
    typesForVisualization.delete(key)
    for (const type of types) {
      visualizationsForType.get(type)?.delete(key)
    }
  }

  function updateVisualizationTypes(id: VisualizationIdentifier, inputType: Opt<string>) {
    const key = toVisualizationCacheKey(id)
    const newTypes = getTypesFromUnion(inputType)
    const types = typesForVisualization.get(key)
    typesForVisualization.set(key, newTypes)
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
    if (name === 'Loading' || name === 'Loading Error') continue
    updateVisualizationTypes(
      {
        module: { kind: 'Builtin' },
        name,
      },
      inputType,
    )
  }

  function types(type: string | undefined) {
    const ret =
      type === undefined
        ? allBuiltinVisualizations
        : [
            ...new Set([
              ...(visualizationsForType.get(type) ?? []),
              ...(visualizationsForType.get('Any') ?? []),
            ]),
          ].map(fromVisualizationCacheKey)
    return ret
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
        case 'Added':
        case 'Modified': {
          try {
            const abortController = new AbortController()
            compilationAbortControllers.set(pathString, abortController)
            const vizPromise = compile(
              'enso-current-project:' + pathString,
              await projectRoot,
              await data,
            )
            if (key) cache.set(key, vizPromise)
            const viz = await vizPromise
            if (abortController.signal.aborted) break
            if (!id || viz.name !== id.name) {
              if (id) removeVisualizationTypes(id)
              const newId: VisualizationIdentifier = {
                module: { kind: 'CurrentProject' },
                name: viz.name,
              }
              updateVisualizationTypes(newId, viz.inputType)
              cache.set(toVisualizationCacheKey(newId), vizPromise)
            } else if (id) {
              updateVisualizationTypes(id, viz.inputType)
            }
          } catch (error) {
            if (key) cache.delete(key)
            if (!(error instanceof InvalidVisualizationModuleError)) {
              console.error('Error loading visualization:')
              console.error(error)
            }
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

  Promise.all([ls, projectRoot]).then(async ([ls, projectRoot]) => {
    if (!projectRoot) {
      console.error('Could not load custom visualizations: File system content root not found.')
      return
    }
    await rpcWithRetries(() =>
      ls.acquireCapability('file/receivesTreeUpdates', {
        path: { rootId: projectRoot, segments: [] } satisfies Path,
      }),
    )
    ls.on('file/event', onFileEvent)
    await walkFiles(ls, { rootId: projectRoot, segments: ['visualizations'] }, (path) =>
      onFileEvent({ path, kind: 'Added' }),
    )
  })

  function get(meta: VisualizationIdentifier, ignoreCache = false) {
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
      const module = await compile(builtinDynamicPath, await projectRoot, await data)
      await loadScripts(module)
      return module
    }
    throw new Error(`Unknown visualization type: ${type}`)
  }

  return { types, get }
})
