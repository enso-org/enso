/* eslint-disable */

import { EnsoPath, ProjectId } from '#/services/Backend'
import { useBackends } from '$/providers/backends'
import { injectOpenedProjects } from '$/providers/openedProjects'
import { createContextStore } from '@/providers'
import { useProjectFiles } from '@/stores/projectFiles'
import { Err, Ok, Result } from '@/util/data/result'
import { computed, effectScope, nextTick, onScopeDispose, ref, Ref, watchEffect } from 'vue'

const [provideEnsoAssetStore, useEnsoAssetStore] = createContextStore(
  'asyncResourceStore',
  (
    openedProjects: ReturnType<typeof injectOpenedProjects>,
    backends: ReturnType<typeof useBackends>,
  ) => {
    // function resolveProjectRelative(project: ProjectId, relativePath: string): EnsoAsset {}

    function resolveAssetInContext(
      unparsedAssetUrl: string,
      context: { project?: ProjectId; modulePathSegments?: string[]; isLocal: boolean },
    ): Result<AsyncResource> {
      const parsedUrl = parseAssetUrl(unparsedAssetUrl, context.modulePathSegments)
      if (!parsedUrl.ok) return Err(parsedUrl.error)
      switch (parsedUrl.value.kind) {
        case 'ensoPath':
          return resolveEnsoPathAsset(parsedUrl.value.ensoPath)
        case 'webUrl':
          return resolveWebAsset(parsedUrl.value.url)
        case 'projectRelative':
          if (context.project == null) return Err('Cannot resolve relative path outside of project')
          return resolveProjectAsset(
            context.project,
            parsedUrl.value.relativePathSegments,
            context.isLocal,
          )
      }
    }

    function resolveEnsoPathAsset(path: EnsoPath): Result<AsyncResource> {
      throw 'unimplemented'
    }

    function resolveWebAsset(url: URL): Result<AsyncResource> {
      throw 'unimplemented'
    }

    function resolveProjectAsset(
      projectId: ProjectId,
      segments: string[],
      isLocal: boolean,
    ): Result<AsyncResource> {
      return Ok(
        new AsyncResource({
          async fetch(abort) {
            const openedProject = openedProjects.get(projectId)
            if (openedProject) {
              // Remote/local projects are treated the same when opened - contact LS for a file.

              const rootId = await openedProject.store.projectRootId
              if (rootId == null) return Err('Could not identify project root.')
              if (abort.aborted) return Err(abort)

              const projectFiles = useProjectFiles(openedProject.store)
              return projectFiles.readFileBinary({ rootId, segments }, abort)
            } else {
              // project not opened

              // TODO: isLocal should be computed from ProjectId
              if (isLocal) {
                let localBackend = backends.localBackend
                if (!localBackend) return Err('Cannot query local resource without local backend.')
                throw 'unimplemented'
              } else {
                let cloudBackend = backends.remoteBackend
                let asset = cloudBackend.resolveProjectAssetData(
                  projectId,
                  segments.join('/'),
                  abort,
                )
              }
              // TODO: unopened remote/hybrid project
              // TODO: unopened local project
              throw 'unimplemented'
            }
          },
        }),
      )
    }

    return {}
  },
)

type ParsedAssetUrl =
  | { kind: 'projectRelative'; relativePathSegments: string[] }
  | { kind: 'ensoPath'; ensoPath: EnsoPath }
  | { kind: 'webUrl'; url: URL } // only allowed web protocols

function parseAssetUrl(
  unparsedAssetUrl: string,
  modulePathSegments?: string[],
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
  } else if (modulePathSegments != null) {
    // We already know that `unparsedAssetUrl` is not a valid URL by itself.
    // Attempt interpreting it as a relative path with project base.
    const asProjectUrl = URL.parse(unparsedAssetUrl, 'project:///' + modulePathSegments.join('/'))
    if (asProjectUrl?.protocol === 'project:') {
      const relativePathSegments = decodeURI(asProjectUrl.pathname).substring(1).split('/')
      return Ok({ kind: 'projectRelative', relativePathSegments })
    }
  }
  return Err('Unsupported asset URL type')
}

interface AssetFetcher {
  fetch(abort: AbortSignal): Promise<Result<Blob>>
}

/**
 * A potentially backend-resolved asset with data.
 *
 * Can be resolved from multiple sources:
 * - http(s):// web URL
 * - enso:// path
 * - Project's local file system
 * */
class AsyncResource {
  scope = effectScope(true)
  fetchInProgress = this.doFetch()
  blobUrlRef: Ref<string | undefined> | undefined

  constructor(private fetcher: AssetFetcher) {}

  doFetch() {
    interface FetchInProgress {
      promise: Promise<Result<Blob>>
      controller: AbortController
    }

    return this.scope.run(() =>
      computed<FetchInProgress>((prev) => {
        prev?.controller.abort('refetch')
        const controller = new AbortController()
        return {
          promise: this.fetcher.fetch(controller.signal),
          controller,
        }
      }),
    )
  }

  state: 'error' | 'uploading' | 'fetching' | 'ready' = 'error'
  getBlobUrl(): string | undefined {
    if (!this.blobUrlRef) {
      this.blobUrlRef = this.scope.run(() =>
        exposeAsyncObjectUrl(() => this.fetchInProgress?.value.promise),
      )
    }
    return this.blobUrlRef?.value
  }
}

function exposeAsyncObjectUrl(
  reactiveGetObject: () => Promise<Result<Blob | MediaSource>> | undefined,
): Ref<string | undefined> {
  const exposedObjectUrl = ref<string>()
  function exposeNewObject(object: Blob | MediaSource) {
    revokeCurrentObject()
    exposedObjectUrl.value = URL.createObjectURL(object)
  }
  function revokeCurrentObject() {
    const url = exposedObjectUrl.value
    if (url != null) {
      exposedObjectUrl.value = undefined
      nextTick(() => URL.revokeObjectURL(url))
    }
  }

  watchEffect((onCleanup) => {
    let cleanedUp = false
    onCleanup(() => (cleanedUp = true))
    reactiveGetObject()?.then((result) => {
      if (!cleanedUp && result.ok) exposeNewObject(result.value)
    })
  })

  onScopeDispose(revokeCurrentObject)
  return exposedObjectUrl
}
