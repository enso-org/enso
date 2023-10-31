import { useProjectStore } from '@/stores/project'
import {
  compile,
  currentProjectProtocol,
  InvalidVisualizationModuleError,
  stylePathAttribute,
  type VisualizationModule,
} from '@/stores/visualization/compilerMessaging'
import {
  fromVisualizationId,
  toVisualizationId,
  VisualiationMetadataDb,
  type VisualizationId,
} from '@/stores/visualization/metadata'
import builtinVisualizationMetadata from '@/stores/visualization/metadata.json'
import { rpcWithRetries } from '@/util/net'
import type { Opt } from '@/util/opt'
import { defineStore } from 'pinia'
import type { Event as LSEvent, VisualizationConfiguration } from 'shared/languageServerTypes'
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

export const useVisualizationStore = defineStore('visualization', () => {
  const cache = reactive(new Map<VisualizationId, Promise<VisualizationModule>>())
  /** A map from file path to {@link AbortController}, so that a file change event can stop previous
   * file change event handlers for the same path. */
  const compilationAbortControllers = reactive(new Map<string, AbortController>())
  /** A map from file path in the current project, to visualization name. This is required so that
   * file delete events can remove the cached visualization. */
  const currentProjectVisualizationsByPath = new Map<string, string>()
  const allBuiltinVisualizations = [
    ...Object.keys(builtinVisualizationImports),
    ...Object.keys(dynamicVisualizationPaths),
  ].map<VisualizationIdentifier>((name) => ({
    module: { kind: 'Builtin' },
    name,
  }))
  const metadata = new VisualiationMetadataDb()
  const proj = useProjectStore()
  const projectRoot = proj.contentRoots.then(
    (roots) => roots.find((root) => root.type === 'Project')?.id,
  )

  for (const vizMetadata of builtinVisualizationMetadata) {
    if (vizMetadata.name === 'Loading' || vizMetadata.name === 'Loading Error') continue
    metadata.set(
      toVisualizationId({ module: { kind: 'Builtin' }, name: vizMetadata.name }),
      vizMetadata,
    )
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
                node.remove()
              })
              node.addEventListener('error', () => {
                reject()
                node.remove()
              })
            }),
          )
          scriptsNode.appendChild(node)
        }
      }
    }
    return Promise.allSettled(promises)
  }

  async function onFileEvent({ kind, path }: LSEvent<'file/event'>) {
    if (path.rootId !== (await projectRoot) || !/\.vue$/.test(path.segments.at(-1) ?? '')) return
    const pathString = path.segments.join('/')
    const name = currentProjectVisualizationsByPath.get(pathString)
    let id: VisualizationIdentifier | undefined =
      name != null ? { module: { kind: 'CurrentProject' }, name } : undefined
    const key = id && toVisualizationId(id)
    compilationAbortControllers.get(pathString)?.abort()
    compilationAbortControllers.delete(pathString)
    switch (kind) {
      case 'Added':
      case 'Modified': {
        try {
          const abortController = new AbortController()
          compilationAbortControllers.set(pathString, abortController)
          const vizPromise = compile(
            'enso-current-project:' + pathString,
            await projectRoot,
            await proj.dataConnection,
          )
          if (key) cache.set(key, vizPromise)
          const viz = await vizPromise
          if (abortController.signal.aborted) break
          if (!id || viz.name !== id.name) {
            id = { module: { kind: 'CurrentProject' }, name: viz.name }
            cache.set(toVisualizationId(id), vizPromise)
          }
          metadata.set(toVisualizationId(id), { name: viz.name, inputType: viz.inputType })
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
        if (key) {
          cache.delete(key)
          metadata.delete(key)
          for (const el of document.querySelectorAll(
            `[${stylePathAttribute}=${currentProjectProtocol}${pathString}]`,
          )) {
            el.remove()
          }
        }
      }
    }
  }

  Promise.all([proj.lsRpcConnection, projectRoot]).then(([ls, projectRoot]) => {
    if (!projectRoot) {
      console.error('Could not load custom visualizations: Project directory not found.')
      return
    }
    ls.watchFiles(projectRoot, ['visualizations'], onFileEvent, rpcWithRetries)
  })

  function types(type: Opt<string>) {
    const ret =
      type == null
        ? allBuiltinVisualizations
        : [
            ...new Set([
              ...(metadata.types.reverseLookup(type) ?? []),
              ...(metadata.types.reverseLookup('Any') ?? []),
            ]),
          ].map(fromVisualizationId)
    return ret
  }

  function get(meta: VisualizationIdentifier, ignoreCache = false) {
    const key = toVisualizationId(meta)
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
    const builtinImport = builtinVisualizationImports[type]?.()
    if (builtinImport) {
      const module = await builtinImport
      await loadScripts(module)
      return module
    }
    const builtinDynamicPath = dynamicVisualizationPaths[type]
    if (builtinDynamicPath) {
      const module = await compile(builtinDynamicPath, await projectRoot, await proj.dataConnection)
      await loadScripts(module)
      return module
    }
    throw new Error(`Unknown visualization type: ${type}`)
  }

  return { types, get }
})
