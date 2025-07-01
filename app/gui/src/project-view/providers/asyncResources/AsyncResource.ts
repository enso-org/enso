import { LRUCache } from '#/utilities/LruCache'
import { Err, Ok, Result } from '@/util/data/result'
import {
  computed,
  EffectScope,
  effectScope,
  getCurrentScope,
  nextTick,
  onScopeDispose,
  ref,
  Ref,
  watchEffect,
} from 'vue'
import { assertDefined } from 'ydoc-shared/util/assert'

export type ResourceKey = string

type ResolvedFetchData = Blob | URL

export type FetchPartialProgress = {
  dataUpdate: ResolvedFetchData
  continue: Promise<FetchResult>
}
export type FetchResult = Result<ResolvedFetchData | FetchPartialProgress>

export interface ResourceDefinition {
  cacheKey: ResourceKey
  uploading?: Promise<FetchResult>
  fetch(abort: AbortSignal): Promise<FetchResult>
}

type FetchInProgress = {
  promise: Promise<FetchResult>
  controller: AbortController
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
  private forceRefetchCounter: Ref<number>
  private paused: Ref<boolean>
  private _status: Ref<'loading' | 'uploading' | 'error' | 'ready'>
  private lastErrorMessage: string | undefined

  /**
   * Create a new `AsyncResource` instance that will use specific fetcher as a strategy to retreive its data.
   * Each new instance starts in unpaused state by default.
   *
   * Note: Every constructed resource must eventually be manually `dispose`d.
   */
  static Create(fetcher: ResourceDefinition): AsyncResource {
    const scope = effectScope(true)
    return scope.run(() => new AsyncResource(fetcher))!
  }

  private constructor(private fetcher: ResourceDefinition) {
    const currentScope = getCurrentScope()
    assertDefined(currentScope)
    this.scope = currentScope
    this.urlRef = ref<string>()
    this.forceRefetchCounter = ref(0)
    this.paused = ref(false)
    this._status = ref(fetcher.uploading ? 'uploading' : 'loading')

    const fetchInProgress = computed<FetchInProgress>((previous) => {
      previous?.controller.abort('refetch')
      const controller = new AbortController()

      // Refetch logic depends on this ref being depended on here, because
      // increments to this counter are what causes the fetch to retrigger.
      const forceRefetchCount = this.forceRefetchCounter.value

      let promise
      if (forceRefetchCount === 0 && fetcher.uploading != null) {
        promise = fetcher.uploading
      } else {
        promise = this.fetcher.fetch(controller.signal)
      }

      return { promise, controller }
    })

    const exposePromise = computed(() => {
      const { promise, controller } = fetchInProgress.value
      const exposeResult = promise.then(async (result) => {
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
      return exposeResult.then((result) => {
        if (result.ok) {
          this._status.value = 'ready'
          return
        }
        if (result.error.payload instanceof AbortSignal) return
        this.lastErrorMessage = result.error.message('')
        this._status.value = 'error'
      })
    })

    watchEffect(() => {
      if (this.paused.value) return
      // When resource is not paused, make sure the fetch chain is running by depending on the `exposePromise` computed value.
      const _ = exposePromise.value
    })

    onScopeDispose(() => this.revokeCurrentObject())
  }

  /**
   * Pause or unpause this resource. Paused resources will not be automatically refetched when their fetcher is invalidated.
   */
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
   * Cause this resource to be refetched from network. Does nothing if the resource is still being uploaded.
   */
  public refresh() {
    if (this.status === 'uploading') {
      // We cannot refetch while uploading is still ongoing, since the place where we fetch from might not have the resource yet.
      return
    }
    this._status.value = 'loading'
    this.forceRefetchCounter.value = this.forceRefetchCounter.value + 1
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
   *
   */
  dispose() {
    if (!this.scope.active) return
    this.scope.stop()
  }
}

/**
 * The maximum count of resources that we will keep around, even if they are no longer being used by any view.
 */
const MAX_CACHED_UNUSED_RESOURCES = 64

/**
 * Create a cache that maintains reference counded resources.
 *
 * Part of 'asyncResources' store.
 * @internal
 */
export function initResourceCache() {
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

  function retainResource(fetcher: ResourceDefinition): AsyncResource {
    const used = usedResources.get(fetcher.cacheKey)
    if (used) {
      used.refcount += 1
      return used.res
    }
    const parkedResource = parkedResources.take(fetcher.cacheKey) ?? AsyncResource.Create(fetcher)
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

  return {
    retainResource,
    releaseResource,
  }
}
