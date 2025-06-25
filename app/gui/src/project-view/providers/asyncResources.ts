/* eslint-disable */

import { EnsoPath, ProjectId } from '#/services/Backend'
import { isLocalProjectId } from '#/services/LocalBackend'
import { LRUCache } from '#/utilities/LruCache'
import { injectCurrentProject } from '$/components/WithCurrentProject.vue'
import { useBackends } from '$/providers/backends'
import { injectOpenedProjects } from '$/providers/openedProjects'
import { createContextStore } from '@/providers'
import { useProjectFiles } from '@/stores/projectFiles'
import { Err, mapOk, Ok, Result } from '@/util/data/result'
import { ToValue } from '@/util/reactivity'
import {
  computed,
  ComputedRef,
  effectScope,
  MaybeRefOrGetter,
  nextTick,
  onScopeDispose,
  ref,
  Ref,
  toValue,
  watchEffect,
} from 'vue'

const MAX_CACHED_UNUSED_RESOURCES = 64

interface ResourceContext {
  project: () => ProjectId | undefined
  modulePathSegments: () => string[] | undefined
}

export const [provideAsyncResources, useAsyncResources] = createContextStore(
  'asyncResourceStore',
  (openedProjects: ReturnType<typeof injectOpenedProjects>) => {
    const backends = useBackends()
    type ResourceKey = ResourceFetcher['cacheKey']
    const usedResources = new Map<ResourceKey, { refcount: number; res: AsyncResource }>()
    const parkedResources = new LRUCache<ResourceKey, AsyncResource>(
      MAX_CACHED_UNUSED_RESOURCES,
      (r) => r.dispose(),
    )

    function unparkResource(key: ResourceKey, res: AsyncResource): AsyncResource {
      res.setPaused(false)
      usedResources.set(key, { refcount: 1, res })
      return res
    }

    function retainResource(fetcher: ResourceFetcher): AsyncResource {
      const used = usedResources.get(fetcher.cacheKey)
      if (used) {
        used.refcount += 1
        return used.res
      }
      const parkedResource = parkedResources.take(fetcher.cacheKey) ?? new AsyncResource(fetcher)
      return unparkResource(fetcher.cacheKey, parkedResource)
    }

    function releaseResource(key: ResourceKey) {
      const used = usedResources.get(key)
      if (!used) return
      used.refcount -= 0
      if (used.refcount <= 0) {
        usedResources.delete(key)
        used.res.setPaused(true)
        parkedResources.set(key, used.res)
      }
    }

    onScopeDispose(() => parkedResources.clear())

    function resolveResourceInContext(
      unparsedAssetUrl: string,
      context: ResourceContext,
    ): Result<ResourceFetcher> {
      const parsedUrl = parseAssetUrl(unparsedAssetUrl, context.modulePathSegments)
      if (!parsedUrl.ok) return Err(parsedUrl.error)
      switch (parsedUrl.value.kind) {
        case 'ensoPath': {
          return Ok(resolveEnsoPathResource(parsedUrl.value.ensoPath))
        }
        case 'webUrl': {
          return Ok(resolveWebResource(parsedUrl.value.url))
        }
        case 'projectRelative': {
          {
            const project = context.project()
            if (project == null) return Err('Cannot resolve relative path outside of project')
            return Ok(resolveProjectResource(project, parsedUrl.value.relativePath))
          }
        }
      }
    }

    function resolveEnsoPathResource(path: EnsoPath): ResourceFetcher {
      return {
        cacheKey: `ensoPath-${path}`,
        async fetch(abort) {
          throw 'unimplemented'
        },
      }
    }

    function resolveWebResource(url: URL): ResourceFetcher {
      return {
        cacheKey: `web-${url}`,
        async fetch(_abort) {
          return Ok(url)
        },
      }
    }

    function resolveProjectResource(projectId: ProjectId, relativePath: string): ResourceFetcher {
      return {
        cacheKey: `projectRelative-${projectId}-${relativePath}`,
        async fetch(abort) {
          const openedProject = openedProjects.get(projectId)
          if (openedProject) {
            // Remote/local projects are treated the same when opened - contact LS for a file.
            const rootId = await openedProject.store.projectRootId
            if (rootId == null) return Err('Could not identify project root.')
            if (abort.aborted) return Err(abort)

            const projectFiles = useProjectFiles(openedProject.store)
            return projectFiles.readFileBinary({ rootId, segments: relativePath.split('/') }, abort)
          } else {
            // project not opened
            if (isLocalProjectId(projectId)) {
              // unopened local project
              let localBackend = backends.localBackend
              if (!localBackend) return Err('Cannot query local resource without local backend.')
              const data = await localBackend.resolveProjectAssetData(projectId, relativePath)
              if (abort.aborted) return Err(abort)
              return Ok(data)
            } else {
              // unopened remote project
              let cloudBackend = backends.remoteBackend
              try {
                return Ok(
                  await cloudBackend.resolveProjectAssetData(projectId, relativePath, abort),
                )
              } catch (e) {
                return Err(e)
              }
            }
          }
        },
      }
    }

    function useAmbinedContext(): ResourceContext {
      const currentProject = injectCurrentProject(true)
      return {
        project: () => currentProject?.id.value ?? undefined,
        modulePathSegments: () =>
          currentProject?.storesRefs.store.value?.observedFileName?.split('/'),
      }
    }

    return {
      useResourceFromUrl(
        unparsedAssetUrl: ToValue<string>,
        context: ResourceContext = useAmbinedContext(),
      ): ComputedRef<Result<AsyncResource>> {
        const resolved = computed(() =>
          resolveResourceInContext(toValue(unparsedAssetUrl), context),
        )

        let previousKey: ResourceKey | null = null
        function releasePrevious() {
          if (previousKey != null) {
            releaseResource(previousKey)
            previousKey = null
          }
        }
        onScopeDispose(releasePrevious)
        return computed<Result<AsyncResource>>(() => {
          const resourceDef = resolved.value
          const retained = mapOk(resourceDef, retainResource)
          releasePrevious()
          return retained
        })
      },
    }
  },
)

type ParsedAssetUrl =
  | { kind: 'projectRelative'; relativePath: string }
  | { kind: 'ensoPath'; ensoPath: EnsoPath }
  | { kind: 'webUrl'; url: URL } // only allowed web protocols

function parseAssetUrl(
  unparsedAssetUrl: string,
  getModulePathSegments: () => string[] | undefined,
): Result<ParsedAssetUrl> {
  const asUrl = URL.parse(unparsedAssetUrl)
  if (asUrl != null) {
    switch (asUrl?.protocol) {
      case 'http:':
      case 'https:':
        return Ok({ kind: 'webUrl', url: asUrl })
      case 'enso:':
        return Ok({ kind: 'ensoPath', ensoPath: EnsoPath(decodeURI(asUrl.href)) })
    }
    return Err('Unsupported URL protocol')
  }
  const segments = getModulePathSegments()
  if (segments != null) {
    // We already know that `unparsedAssetUrl` is not a valid URL by itself.
    // Attempt interpreting it as a relative path with project base.
    const asProjectUrl = URL.parse(unparsedAssetUrl, 'project:///' + segments.join('/'))
    if (asProjectUrl?.protocol === 'project:') {
      const relativePath = decodeURI(asProjectUrl.pathname).substring(1)
      return Ok({ kind: 'projectRelative', relativePath })
    }
  }
  return Err('Unsupported asset URL type')
}

interface ResourceFetcher {
  cacheKey: string
  // TODO: handle upload flow where we have blob before being ready
  fetch(abort: AbortSignal): Promise<Result<Blob | URL>>
}

/**
 * A potentially backend-resolved asset with data.
 *
 * Can be resolved from multiple sources:
 * - http(s):// web URL
 * - enso:// path
 * - Project's local file system
 */
export class AsyncResource {
  private scope = effectScope(true)
  private urlRef: Ref<string | undefined> = this.scope.run(ref<string>)!
  private createdObjectUrl: string | undefined
  private paused = this.scope.run(() => ref(false))!
  private _status = this.scope.run(() => ref<'loading' | 'error' | 'ready'>('loading'))!

  constructor(private fetcher: ResourceFetcher) {
    this.scope.run(() => {
      type FetchInProgress = {
        promise: ReturnType<ResourceFetcher['fetch']>
        controller: AbortController
      }

      const fetchInProgress = computed<FetchInProgress>((previous) => {
        previous?.controller.abort('refetch')
        const controller = new AbortController()
        const promise = this.fetcher.fetch(controller.signal)
        return { promise, controller }
      })

      const exposePromise = computed(() => {
        const { promise, controller } = fetchInProgress.value
        const exposeResult = promise.then((result) => {
          if (controller.signal.aborted) return Err(controller.signal)
          if (!result.ok) return result
          if (result.value instanceof URL) this.exposeNewUrl(result.value)
          else this.exposeNewObject(result.value)
          return Ok()
        })
        return exposeResult.then((result) => {
          if (result.ok) {
            this._status.value = 'ready'
            return
          }
          if (result.error.payload instanceof AbortSignal) return
          result.error.log('Failed to expose AsyncResource')
          this._status.value = 'error'
        })
      })

      watchEffect(() => {
        if (this.paused.value) return
        // When resource is not paused, make sure the fetch chain is running by depending on the promise computed value.
        let _ = exposePromise.value
      })

      onScopeDispose(() => this.revokeCurrentObject())
    })
  }

  setPaused(paused: boolean) {
    this.paused.value = paused
  }

  private revokeCurrentObject() {
    const url = this.createdObjectUrl
    if (url != null) {
      this.createdObjectUrl = undefined
      nextTick(() => URL.revokeObjectURL(url))
    }
  }

  private exposeNewUrl(url: URL) {
    if (!this.scope.active) return
    this.revokeCurrentObject()
    this.urlRef.value = url.href
  }

  private exposeNewObject(object: File | Blob | MediaSource) {
    if (!this.scope.active) return
    this.revokeCurrentObject()
    this.urlRef.value = this.createdObjectUrl = URL.createObjectURL(object)
  }

  /**
   * Get the URL representing the resource data. Can either be a public http URL, or an `blob://` object url.
   */
  get url(): string | undefined {
    // Depend on fetch computed value
    return this.urlRef.value
  }

  get status() {
    return this._status.value
  }

  dispose() {
    if (!this.scope.active) return
    this.scope.stop()
  }
}

// Composable representing a usage of a given AsyncResource.
export function useAsyncResourceUrl(source: MaybeRefOrGetter<AsyncResource>) {}
