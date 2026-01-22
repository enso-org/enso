import { LRUCache } from '#/utilities/LruCache'
import { Err, Ok, type Result } from 'enso-common/src/utilities/data/result'
import {
  EffectScope,
  effectScope,
  getCurrentScope,
  nextTick,
  onScopeDispose,
  ref,
  watchEffect,
  type Ref,
  type WatchHandle,
} from 'vue'

export type ResourceKey = string

type ResolvedFetchData = Blob | URL

export type FetchPartialProgress = {
  dataUpdate: ResolvedFetchData
  continue: Promise<FetchResult>
}
export type FetchResult = Result<ResolvedFetchData | FetchPartialProgress>

export interface ResourceDefinition {
  cacheKey: ResourceKey
  uploading?: Promise<FetchResult> | 'remote' | undefined
  onCacheHit?(resource: AsyncResource): void
  fetch(abort: AbortSignal): Promise<FetchResult>
}

/**
 * A resource with data that can be fetched from various sources.
 *
 * Can be resolved from multiple sources:
 * - http(s):// web URL
 * - enso:// path
 * - Project's local file system
 */
export class AsyncResource {
  private scope: EffectScope
  private urlRef: Ref<string | undefined>
  private createdObjectUrl: string | undefined
  private refetchCount: Ref<number>
  private _status: Ref<'loading' | 'uploading' | 'error' | 'ready'>
  private lastErrorMessage: string | undefined
  private fetchEffectHandle: WatchHandle | undefined

  /**
   * Create a new `AsyncResource` instance that will use specific fetcher as a strategy to retreive its data.
   * Each new instance starts in unpaused state by default.
   *
   * Note: Every constructed resource must eventually be manually `dispose`d.
   */
  constructor(private fetcher: ResourceDefinition) {
    this.scope = effectScope(true)
    this.urlRef = ref<string>()
    this.refetchCount = ref(0)
    this._status = ref(fetcher.uploading ? 'uploading' : 'loading')

    // Run the actual fetch logic *reactively* inside a dedicated scope. The fetch will be automatically
    // aborted and restarted if the fetch function dependencies were modified.
    this.scope.run(() => {
      this.fetchEffectHandle = watchEffect((onInvalidate) => {
        const controller = new AbortController()
        onInvalidate(() => controller.abort('invalidate'))

        // Refetch logic depends on this ref being depended on here, because
        // increments to this counter are what causes the fetch to retrigger.
        const forceRefetchCount = this.refetchCount.value

        // Attempt to use the uploaded data, unless a refetch was explicitly requested.
        const fetchPromise =
          forceRefetchCount === 0 && fetcher.uploading != null ?
            fetcher.uploading
          : this.fetcher.fetch(controller.signal)

        // Resource is being uploaded by another client, we have no idea when it ends. Wait for refetch.
        if (fetchPromise === 'remote') return

        // NOTE: things happening asynchronously inside `then` call are *not* depended on for the purposes of the
        // watcher. This is intentional, we only want to refetch if the origianl `fetch` dependencies change.

        // Handle partial results in series, return data only once the final result is received.
        const progressPromise = fetchPromise.then(async (result) => {
          while (true) {
            if (controller.signal.aborted) return Err(controller.signal)
            if (!result.ok) return result
            const data = 'dataUpdate' in result.value ? result.value.dataUpdate : result.value
            if (data instanceof URL) this.exposeNewUrl(data)
            else this.exposeNewObject(data)
            if ('continue' in result.value) {
              result = await result.value.continue
            } else {
              return Ok()
            }
          }
        })

        // Once the fetch has concluded, update the resource status accordingly.
        progressPromise.then((result) => {
          if (result.ok) {
            this._status.value = 'ready'
          } else {
            // Be careful to not modify the status if the previous fetch has been aborted.
            // That means either a refetch is in progress, or this resource was disposed.
            if (result.error.payload instanceof AbortSignal) return
            this.lastErrorMessage = result.error.message('')
            this._status.value = 'error'
          }
        })
      })
      onScopeDispose(() => this.revokeCurrentObject())
    })
  }

  /**
   * Pause or unpause this resource. Paused resources will not be automatically refetched when their fetcher is invalidated.
   */
  setPaused(paused: boolean) {
    if (paused) {
      this.fetchEffectHandle?.pause()
    } else {
      this.fetchEffectHandle?.resume()
    }
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
   * Cause this resource to be refetched from network. Does nothing if the resource is still being uploaded.
   * If the resource is paused, it will be scheduled for refetch next time it is unpaused.
   */
  public refresh() {
    // We cannot refetch while uploading is still ongoing, since the place where we fetch from might not have the resource yet.
    if (!this.scope.active || this.status === 'uploading') return
    this.doReload()
  }

  /** Signal that a 'remote' upload was finished. It is safe to refetch data from remote server. */
  public remoteUploadFinished() {
    if (!this.scope.active || this.status !== 'uploading' || this.fetcher.uploading !== 'remote')
      return
    this.doReload()
  }

  private doReload() {
    this._status.value = 'loading'
    this.refetchCount.value = this.refetchCount.value + 1
  }

  /**
   * Get the URL representing the resource data. Can either be a public http URL, or an `blob://` object url.
   */
  get url(): string | undefined {
    // Depend on fetch computed value
    return this.urlRef.value
  }

  /** Current network status of the resource. When `error`, check the 'error' field for message  */
  get status() {
    return this._status.value
  }

  /** The error message. Only returns valid value when resource has a status of `'error'`. */
  get error(): string | undefined {
    if (this.status === 'error') return this.lastErrorMessage
  }

  /**
   * Stop refetching logic and clean up all memory used by this resource.
   */
  dispose() {
    this.scope.stop()
  }
}

/**
 * The maximum count of resources that we will keep around, even if they are no longer being used by any view.
 */
const MAX_CACHED_UNUSED_RESOURCES = 64

/**
 * Create a cache that maintains reference counted resources.
 *
 * Part of 'asyncResources' store.
 * @internal
 */
export function useResourceCache() {
  const usedResources = new Map<ResourceKey, { refcount: number; res: AsyncResource }>()
  const parkedResources = new LRUCache<ResourceKey, AsyncResource>(
    MAX_CACHED_UNUSED_RESOURCES,
    (r) => r.dispose(),
  )
  onScopeDispose(() => parkedResources.clear())

  function unparkResource(key: ResourceKey, res: AsyncResource): AsyncResource {
    res.setPaused(false)
    usedResources.set(key, { refcount: 1, res })
    return res
  }

  function retainResource(fetcher: ResourceDefinition): AsyncResource {
    const used = usedResources.get(fetcher.cacheKey)
    if (used) {
      used.refcount += 1
      fetcher.onCacheHit?.(used.res)
      return used.res
    }
    const parked = parkedResources.take(fetcher.cacheKey)
    if (parked) fetcher.onCacheHit?.(parked)
    return unparkResource(fetcher.cacheKey, parked ?? new AsyncResource(fetcher))
  }

  const scope = getCurrentScope()

  function releaseResource(key: ResourceKey) {
    const used = usedResources.get(key)
    if (used != null && --used.refcount <= 0) {
      usedResources.delete(key)
      // Parked cache only functions as long as the containing scope is not disposed.
      if (scope?.active) {
        used.res.setPaused(true)
        parkedResources.set(key, used.res)
      } else used.res.dispose()
    }
  }

  return {
    retainResource,
    releaseResource,
  }
}
