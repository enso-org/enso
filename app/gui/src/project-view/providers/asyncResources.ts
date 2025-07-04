import { useBackends } from '$/providers/backends'
import { OpenedProjectsStore } from '$/providers/openedProjects'
import { createContextStore } from '@/providers'
import { andThen, mapOk, Ok, Result } from '@/util/data/result'
import { ToValue } from '@/util/reactivity'
import { computed, ComputedRef, onScopeDispose, toValue } from 'vue'
import {
  AsyncResource,
  initResourceCache,
  ResourceDefinition,
  ResourceKey,
} from './asyncResources/AsyncResource'
import {
  capturedContextAsLazy,
  CapturedResourceContext,
  captureResourceContext,
  ResourceContext,
  useAmbientContext,
} from './asyncResources/context'
import { initAsyncResourceResolver } from './asyncResources/resolve'
import {
  AnyUploadSource,
  initResourceUpload,
  normalizeUploadSources,
  uploadAsFetchProgress,
  UploadDefinition,
  UploadProgress,
} from './asyncResources/upload'

export const [provideAsyncResources, useAsyncResources] = createContextStore(
  'asyncResourceStore',
  (openedProjects: OpenedProjectsStore) => {
    const backends = useBackends()
    const { retainResource, releaseResource } = initResourceCache()
    const resolveResourceInContext = initAsyncResourceResolver(backends, openedProjects)
    const uploadResource = initResourceUpload(openedProjects)

    function finishResourceUpload(
      progress: UploadProgress,
      context: CapturedResourceContext,
    ): Result<string> {
      const resolvedDefinition = resolveResourceInContext(
        progress.unparsedResourceUrl,
        capturedContextAsLazy(context),
      )
      if (!resolvedDefinition.ok) return resolvedDefinition

      const uploadDefinition: ResourceDefinition = {
        ...resolvedDefinition.value,
        uploading: uploadAsFetchProgress(progress),
      }

      // Put the resource into cache, but ensure that it is not being flagged as actively used.
      retainResource(uploadDefinition)
      releaseResource(uploadDefinition.cacheKey)

      // Finally, return an resource URL that can be used to retrieve the uploaded resource.
      return Ok(progress.unparsedResourceUrl)
    }

    async function uploadSingleResource(
      definition: UploadDefinition,
      context: CapturedResourceContext,
    ) {
      const progress = await uploadResource(definition, context)
      return andThen(progress, (p) => finishResourceUpload(p, context))
    }

    return {
      /**
       * Add a usage point for a resource represented by given reactive URL.
       * Resources returned by this are automatically considered "used" as long as
       * this composable's scope is alive. Previously downloaded and currently unused
       * resources will stay around in cache up to a limit, until they are eventually
       * dropped and would have to be redownloaded when requested again.
       *
       * Resources that are currently considered "used" are not counting towards the
       * cache size limit.
       */
      useResourceFromUrl(
        unparsedResourceUrl: ToValue<string>,
        context: ResourceContext = useAmbientContext(),
      ): ComputedRef<Result<AsyncResource>> {
        const resolved = computed(() =>
          resolveResourceInContext(toValue(unparsedResourceUrl), context),
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
          if (resourceDef.ok) previousKey = resourceDef.value.cacheKey
          return retained
        })
      },

      /**
       * Try uploading files and create resource objects from them.
       * @returns resource URLs to pass into `useResourceFromUrl` to resolve uploaded assets.
       */
      uploadResources(
        source: AnyUploadSource,
        context: ResourceContext,
      ): Array<Promise<Result<{ filename: string; resourceUrl: string }>>> {
        const capturedContext = captureResourceContext(context)
        // Start all uploads immediately, but yield them in original order.
        const normalizedSources = [...normalizeUploadSources(source)]
        return normalizedSources.map((s) =>
          uploadSingleResource(s, capturedContext).then((upload) =>
            mapOk(upload, (resourceUrl) => ({
              filename: s.filename,
              resourceUrl,
            })),
          ),
        )
      },
    }
  },
)
