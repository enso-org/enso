import { useBackends } from '$/providers/backends'
import type { OpenedProjectsStore } from '$/providers/openedProjects'
import { createContextStore } from '@/providers'
import type { ToValue } from '@/util/reactivity'
import { useQueryClient } from '@tanstack/vue-query'
import { andThen, mapOk, Ok, type Result } from 'enso-common/src/utilities/data/result'
import { computed, onScopeDispose, toValue, type ComputedRef } from 'vue'
import {
  AsyncResource,
  useResourceCache,
  type ResourceDefinition,
  type ResourceKey,
} from './asyncResources/AsyncResource'
import {
  captureResourceContext,
  useCurrentProjectResourceContext,
  type ResourceContext,
  type ResourceContextSnapshot,
} from './asyncResources/context'
import { useAsyncResourceResolver } from './asyncResources/resolve'
import {
  normalizeUploadSources,
  uploadAsFetchProgress,
  useResourceUpload,
  type AnyUploadSource,
  type UploadDefinition,
  type UploadProgress,
} from './asyncResources/upload'

export type AsyncResourceStore = ReturnType<typeof useAsyncResources>

export type StartedUpload = Result<{
  filename: string
  resourceUrl: string
  complete: Promise<Result>
}>

export const [provideAsyncResources, useAsyncResources] = createContextStore(
  'asyncResourceStore',
  (openedProjects: OpenedProjectsStore) => {
    const backends = useBackends()
    const queryClient = useQueryClient()
    const { retainResource, releaseResource } = useResourceCache()
    const resolveResourceInContext = useAsyncResourceResolver(backends, openedProjects)
    const uploadResource = useResourceUpload(openedProjects, backends.remoteBackend, queryClient)

    function finishResourceUpload(
      progress: UploadProgress,
      context: ResourceContextSnapshot,
    ): Result<{ resourceUrl: string; upload: Promise<Result> }> {
      const resolvedDefinition = resolveResourceInContext(progress.resourceUrl, context)
      if (!resolvedDefinition.ok) return resolvedDefinition

      const uploadDefinition: ResourceDefinition = {
        ...resolvedDefinition.value,
        uploading: uploadAsFetchProgress(progress),
      }

      // Put the resource into cache, but ensure that it is not being flagged as actively used.
      retainResource(uploadDefinition)
      releaseResource(uploadDefinition.cacheKey)

      // Finally, return an resource URL that can be used to retrieve the uploaded resource.
      return Ok({
        resourceUrl: progress.resourceUrl,
        upload: progress.upload,
      })
    }

    async function uploadSingleResource(
      definition: UploadDefinition,
      context: ResourceContextSnapshot,
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
        resourceUrl: ToValue<string>,
        context: ResourceContext = useCurrentProjectResourceContext(),
      ): ComputedRef<Result<AsyncResource>> {
        const resolved = computed(() => resolveResourceInContext(toValue(resourceUrl), context))

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
      uploadResources(source: AnyUploadSource, context: ResourceContext): Promise<StartedUpload>[] {
        const capturedContext = captureResourceContext(context)
        // Start all uploads immediately, but yield them in original order.
        const normalizedSources = [...normalizeUploadSources(source)]
        return normalizedSources.map((s) =>
          uploadSingleResource(s, capturedContext).then((upload) =>
            mapOk(upload, ({ resourceUrl, upload }) => ({
              filename: s.filename,
              resourceUrl,
              complete: upload,
            })),
          ),
        )
      },
    }
  },
)
