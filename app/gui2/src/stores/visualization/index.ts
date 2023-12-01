import * as geoMapVisualization from '@/components/visualizations/GeoMapVisualization.vue'
import * as heatmapVisualization from '@/components/visualizations/HeatmapVisualization.vue'
import * as histogramVisualization from '@/components/visualizations/HistogramVisualization.vue'
import * as imageBase64Visualization from '@/components/visualizations/ImageBase64Visualization.vue'
import * as jsonVisualization from '@/components/visualizations/JSONVisualization.vue'
import * as scatterplotVisualization from '@/components/visualizations/ScatterplotVisualization.vue'
import * as sqlVisualization from '@/components/visualizations/SQLVisualization.vue'
import * as tableVisualization from '@/components/visualizations/TableVisualization.vue'
import * as warningsVisualization from '@/components/visualizations/WarningsVisualization.vue'
import { useProjectStore } from '@/stores/project'
import {
  compile,
  currentProjectProtocol,
  InvalidVisualizationModuleError,
  stylePathAttribute,
} from '@/stores/visualization/compilerMessaging'
import {
  fromVisualizationId,
  toVisualizationId,
  VisualizationMetadataDb,
  type VisualizationId,
} from '@/stores/visualization/metadata'
import type { VisualizationModule } from '@/stores/visualization/runtimeTypes'
import { rpcWithRetries } from '@/util/net'
import type { Opt } from '@/util/opt'
import { defineStore } from 'pinia'
import type { Event as LSEvent, VisualizationConfiguration } from 'shared/languageServerTypes'
import type { VisualizationIdentifier } from 'shared/yjsModel'
import { computed, reactive } from 'vue'

/** The directory in the project under which custom visualizations can be found. */
const customVisualizationsDirectory = 'visualizations'

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

const builtinVisualizations: VisualizationModule[] = [
  jsonVisualization,
  tableVisualization,
  scatterplotVisualization,
  histogramVisualization,
  heatmapVisualization,
  sqlVisualization,
  geoMapVisualization,
  imageBase64Visualization,
  warningsVisualization,
]

const builtinVisualizationsByName = Object.fromEntries(
  builtinVisualizations.map((viz) => [viz.name, viz]),
)

export const useVisualizationStore = defineStore('visualization', () => {
  const cache = reactive(new Map<VisualizationId, Promise<VisualizationModule>>())
  /** A map from file path to {@link AbortController}, so that a file change event can stop previous
   * file change event handlers for the same path. */
  const compilationAbortControllers = reactive(new Map<string, AbortController>())
  /** A map from file path in the current project, to visualization name. This is required so that
   * file delete events can remove the cached visualization. */
  const currentProjectVisualizationsByPath = new Map<string, string>()
  const metadata = new VisualizationMetadataDb()
  const proj = useProjectStore()
  const projectRoot = proj.contentRoots.then(
    (roots) => roots.find((root) => root.type === 'Project')?.id,
  )

  for (const { name, inputType, icon } of builtinVisualizations) {
    metadata.set(toVisualizationId({ module: { kind: 'Builtin' }, name }), {
      name,
      inputType,
      icon,
    })
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
          // Some resources still set only "Access-Control-Allow-Origin" in the response.
          // We need to explicitly make a request CORS - see https://resourcepolicy.fyi
          node.crossOrigin = 'anonymous'
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
    if (path.rootId !== (await projectRoot) || !/[.]vue$/.test(path.segments.at(-1) ?? '')) return
    const pathString = customVisualizationsDirectory + '/' + path.segments.join('/')
    const name = currentProjectVisualizationsByPath.get(pathString)
    let id: VisualizationIdentifier | undefined =
      name != null ? { module: { kind: 'CurrentProject' }, name } : undefined
    const key = id && toVisualizationId(id)
    compilationAbortControllers.get(pathString)?.abort()
    compilationAbortControllers.delete(pathString)
    // FIXME [sb]: Ideally these should be deleted as late as possible, instead of immediately.
    for (const el of document.querySelectorAll(
      `[${stylePathAttribute}="${CSS.escape(currentProjectProtocol + pathString)}"]`,
    )) {
      el.remove()
    }
    switch (kind) {
      case 'Added':
      case 'Modified': {
        try {
          const abortController = new AbortController()
          compilationAbortControllers.set(pathString, abortController)
          const vizPromise = compile(
            currentProjectProtocol + pathString,
            await projectRoot,
            await proj.dataConnection,
          ).then(async (viz) => {
            await loadScripts(viz)
            return viz
          })
          if (key) cache.set(key, vizPromise)
          const viz = await vizPromise
          if (abortController.signal.aborted) break
          currentProjectVisualizationsByPath.set(pathString, viz.name)
          if (!id || viz.name !== id.name) {
            if (key && id && viz.name !== id.name) {
              cache.delete(key)
              metadata.delete(key)
            }
            id = { module: { kind: 'CurrentProject' }, name: viz.name }
            cache.set(toVisualizationId(id), vizPromise)
          }
          metadata.set(toVisualizationId(id), {
            name: viz.name,
            inputType: viz.inputType,
            icon: viz.icon,
          })
        } catch (error) {
          if (key) cache.delete(key)
          if (error instanceof InvalidVisualizationModuleError) {
            console.info(
              `Imported local file '${pathString}' which is not a visualization. ` +
                `If it is not a dependency, are you perhaps missing \`name\` or \`inputType\`?`,
            )
          } else {
            console.error('Could not load visualization:', error)
          }
        }
        break
      }
      case 'Removed': {
        currentProjectVisualizationsByPath.delete(pathString)
        if (key) {
          cache.delete(key)
          metadata.delete(key)
        }
      }
    }
  }

  Promise.all([proj.lsRpcConnection, projectRoot]).then(([ls, projectRoot]) => {
    if (!projectRoot) {
      console.error('Could not load custom visualizations: Project directory not found.')
      return
    }
    ls.watchFiles(projectRoot, [customVisualizationsDirectory], onFileEvent, rpcWithRetries)
  })

  function* types(type: Opt<string>) {
    const types =
      type == null
        ? metadata.keys()
        : new Set([
            ...(metadata.visualizationIdToType.reverseLookup(type) ?? []),
            ...(metadata.visualizationIdToType.reverseLookup('Any') ?? []),
          ])
    for (const type of types) yield fromVisualizationId(type)
  }

  function icon(type: VisualizationIdentifier) {
    return metadata.get(toVisualizationId(type))?.icon
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
    const module = builtinVisualizationsByName[type]
    if (!module) throw new Error(`Unknown visualization type: ${type}`)
    await loadScripts(module)
    return module
  }

  return { types, get, icon }
})
