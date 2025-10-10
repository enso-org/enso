import { backendMutationOptions } from '#/hooks/backendHooks'
import { createGetProjectDetailsQuery, getProjectDetailsQueryKey } from '#/hooks/projectHooks'
import { setModal } from '#/providers/ModalProvider'
import Backend, {
  AssetType,
  BackendType,
  EnsoPath,
  extractTypeAndPath,
  IS_OPENING_OR_OPENED,
  Plan,
  ProjectState,
  S3_CHUNK_SIZE_BYTES,
  type AssetId,
  type DirectoryId,
  type ListDirectoryResponseBody,
  type Project,
  type ProjectAsset,
  type UploadedAsset,
  type UploadFileRequestParams,
} from '#/services/Backend'
import { assert } from '#/utilities/error'
import { noop } from '#/utilities/functions'
import { merge } from '#/utilities/object'
import { ConfirmCloseModal } from '#/utilities/preventNavigation'
import { useVueEnsureQueryData } from '#/utilities/tanstackQuery'
import { useAuth } from '$/providers/auth'
import { useBackends } from '$/providers/backends'
import { useContainerData, type LaunchedProject } from '$/providers/container'
import { useFeatureFlag } from '$/providers/featureFlags'
import { useText } from '$/providers/text'
import {
  QueryClient,
  useMutation,
  useQueryClient,
  type InfiniteData,
  type QueryKey,
  type UseMutationReturnType,
} from '@tanstack/vue-query'
import { isOnElectron } from 'enso-common/src/detect'
import { uniqueString } from 'enso-common/src/utilities/uniqueString'
import { createElement } from 'react'
import invariant from 'tiny-invariant'
import { computed, ref, toValue, watchEffect, type MaybeRef, type Ref } from 'vue'
import z from 'zod'

/** The delay, in milliseconds, before query data for a file being uploaded is cleared. */
const CLEAR_PROGRESS_DELAY_MS = 5_000
const UPLOADING_FILES_QUERY_KEY = ['uploadingFiles'] satisfies QueryKey
const OPEN_IN_PROGRESS_PROJECT_STATE_SCHEMA = z.object({
  state: z.object({ type: z.literal(ProjectState.openInProgress) }),
})

/** Options for {@link usePreventNavigation}. */
interface PreventNavigationOptions {
  readonly isEnabled?: MaybeRef<boolean>
  readonly message: MaybeRef<string>
}

let shouldClose = false

/** Prevent navigating away from a page. */
function usePreventNavigation({ isEnabled = true, message }: PreventNavigationOptions) {
  watchEffect(() => {
    if (toValue(isEnabled)) {
      const onBeforeUnload = (event: BeforeUnloadEvent) => {
        if (!isOnElectron()) {
          // Browsers have their own `beforeunload` handling.
          event.preventDefault()
        } else if (!shouldClose) {
          event.preventDefault()
          setModal(createElement(ConfirmCloseModal, { message: toValue(message) }))
        } else {
          // Allow the window to close. Set `shouldClose` to false just in case something goes wrong.
          shouldClose = false
        }
      }
      window.addEventListener('beforeunload', onBeforeUnload)
      return () => {
        window.removeEventListener('beforeunload', onBeforeUnload)
      }
    }
  })
}

/** Upload progress for {@link useUploadFileMutation}. */
export interface UploadFileMutationProgress {
  /**
   * Whether this is the first progress update.
   * Useful to determine whether to create a new toast or to update an existing toast.
   */
  readonly event: 'begin' | 'chunk' | 'end'
  readonly sentBytes: number
  readonly totalBytes: number
}

/** Options for {@link useUploadFileMutation}. */
export interface UploadFileMutationOptions {
  /** Defaults to `true`. */
  readonly updateProgress?: boolean | undefined
  /**
   * Defaults to `3`.
   * Controls the default value of {@link UploadFileMutationOptions['chunkRetries']}
   * and {@link UploadFileMutationOptions['endRetries']}.
   */
  readonly retries?: number | undefined
  /** Defaults to {@link UploadFileMutationOptions['retries']}. */
  readonly chunkRetries?: number | undefined
  /** Defaults to {@link UploadFileMutationOptions['retries']}. */
  readonly endRetries?: number | undefined
  /** Called for all progress updates (`onBegin`, `onChunkSuccess` and `onSuccess`). */
  readonly onProgress?: ((progress: UploadFileMutationProgress) => void) | undefined
  /** Called before any mutations are sent. */
  readonly onBegin?: ((progress: UploadFileMutationProgress) => void) | undefined
  /** Called after each successful chunk upload mutation. */
  readonly onChunkSuccess?: ((progress: UploadFileMutationProgress) => void) | undefined
  /** Called after the entire mutation succeeds. */
  readonly onSuccess?: ((progress: UploadFileMutationProgress) => void) | undefined
  /** Called after any mutations fail. */
  readonly onError?: ((error: unknown) => void) | undefined
  /** Called after `onSuccess` or `onError`, depending on whether the mutation succeeded. */
  readonly onSettled?:
    | ((progress: UploadFileMutationProgress | null, error: unknown) => void)
    | undefined
}

/** The result of a {@link useUploadFileMutation}. */
export type UploadFileMutationResult = UseMutationReturnType<
  UploadedAsset,
  Error,
  [body: UploadFileRequestParams, file: File],
  unknown
> & { readonly sentBytes: Ref<number>; readonly totalBytes: Ref<number> }

/** A key for an "uploading file" computed query. */
export function uploadingFilesQueryKey() {
  return UPLOADING_FILES_QUERY_KEY
}

/** Set the progress of a file upload. */
function setUploadingFileProgress(
  queryClient: QueryClient,
  id: string,
  progress: UploadFileMutationProgress,
) {
  queryClient.setQueryData<Record<string, UploadFileMutationProgress>>(
    uploadingFilesQueryKey(),
    (data) => ({ ...data, [id]: progress }),
  )
}

/** Clear the progress of file uploads if all current file uploads are done. */
function clearUploadingFileProgressIfDone(queryClient: QueryClient) {
  queryClient.setQueryData<Record<string, UploadFileMutationProgress>>(
    uploadingFilesQueryKey(),
    (data) => {
      if (!data) {
        return
      }
      for (const [, progress] of Object.entries(data)) {
        if (progress.event !== 'end') {
          return
        }
      }
      return {}
    },
  )
}

/**
 * Call "upload file" mutations for a file.
 * Always uses multipart upload for Cloud backend.
 */
function useUploadFile(backend: Backend, options: UploadFileMutationOptions = {}) {
  const queryClient = useQueryClient()
  const { getText } = useText()
  const fileChunkUploadPoolSize = useFeatureFlag('fileChunkUploadPoolSize')
  const {
    retries = 3,
    chunkRetries = retries,
    endRetries = retries,
    updateProgress = true,
    onError = () => {
      console.error(getText('uploadLargeFileError'))
    },
  } = options
  const setProgress: typeof setUploadingFileProgress =
    updateProgress ? setUploadingFileProgress : noop
  const uploadFileStart = useMutation(
    backendMutationOptions(backend, 'uploadFileStart'),
  ).mutateAsync
  const isPending = ref(false)
  const uploadFileChunk = useMutation(
    backendMutationOptions(backend, 'uploadFileChunk', {
      retry: chunkRetries,
      meta: { pool: { id: 'uploadFileChunk', parallelism: fileChunkUploadPoolSize.value } },
    }),
  ).mutateAsync
  const uploadFileEnd = useMutation(
    backendMutationOptions(backend, 'uploadFileEnd', { retry: endRetries }),
  ).mutateAsync

  usePreventNavigation({ message: getText('anUploadIsInProgress'), isEnabled: isPending })

  return async ([body, file]: [body: UploadFileRequestParams, file: File]) => {
    isPending.value = true
    const progressId = uniqueString()
    const fileSizeBytes = file.size
    const beginProgress: UploadFileMutationProgress = {
      event: 'begin',
      sentBytes: 0,
      totalBytes: fileSizeBytes,
    }
    options.onBegin?.(beginProgress)
    setProgress(queryClient, progressId, beginProgress)
    try {
      const { sourcePath, uploadId, presignedUrls } = await uploadFileStart([body, file])
      let completedChunkCount = 0
      const parts = await Promise.all(
        presignedUrls.map((url, i) =>
          uploadFileChunk([url, file, i]).then((part) => {
            // This cannot be the `onSuccess` callback in `mutateAsync` because then it would not run
            // if the component is unmounted beforehand (which seems to be the case?).
            completedChunkCount += 1
            const newSentBytes = Math.min(completedChunkCount * S3_CHUNK_SIZE_BYTES, fileSizeBytes)
            const chunkProgress: UploadFileMutationProgress = {
              event: 'chunk',
              sentBytes: newSentBytes,
              totalBytes: fileSizeBytes,
            }
            options.onChunkSuccess?.(chunkProgress)
            setProgress(queryClient, progressId, chunkProgress)
            return part
          }),
        ),
      )
      const result = await uploadFileEnd([
        {
          parentDirectoryId: body.parentDirectoryId,
          parts,
          sourcePath: sourcePath,
          uploadId: uploadId,
          assetId: body.fileId,
          fileName: body.fileName,
        },
      ])
      const endProgress: UploadFileMutationProgress = {
        event: 'end',
        sentBytes: fileSizeBytes,
        totalBytes: fileSizeBytes,
      }
      options.onSuccess?.(endProgress)
      options.onSettled?.(endProgress, null)
      setProgress(queryClient, progressId, endProgress)
      if (updateProgress) {
        setTimeout(() => {
          clearUploadingFileProgressIfDone(queryClient)
        }, CLEAR_PROGRESS_DELAY_MS)
      }
      return result
    } catch (error) {
      onError(error)
      options.onSettled?.(null, error)
      throw error
    } finally {
      isPending.value = false
    }
  }
}

/** Return a function to update a project asset in the TanStack Query cache. */
function useSetProjectAsset() {
  const queryClient = useQueryClient()
  return (
    backendType: BackendType,
    assetId: AssetId,
    parentId: DirectoryId,
    transform: (asset: ProjectAsset) => ProjectAsset,
  ) => {
    const listDirectoryQuery = queryClient
      .getQueryCache()
      .find<ListDirectoryResponseBody | undefined>({
        queryKey: [backendType, 'listDirectory', parentId, { infinite: false }],
        exact: false,
      })

    if (listDirectoryQuery?.state.data) {
      listDirectoryQuery.setData({
        ...listDirectoryQuery.state.data,
        assets: listDirectoryQuery.state.data.assets.map((child) =>
          child.id === assetId && child.type === AssetType.project ? transform(child) : child,
        ),
      })
    }

    const listDirectoryInfiniteQuery = queryClient
      .getQueryCache()
      .find<InfiniteData<ListDirectoryResponseBody> | undefined>({
        queryKey: [backendType, 'listDirectory', parentId, { infinite: true }],
        exact: false,
      })

    if (listDirectoryInfiniteQuery?.state.data) {
      listDirectoryInfiniteQuery.setData({
        ...listDirectoryInfiniteQuery.state.data,
        pages: listDirectoryInfiniteQuery.state.data.pages.map((page) => ({
          ...page,
          assets: page.assets.map((child) =>
            child.id === assetId && child.type === AssetType.project ? transform(child) : child,
          ),
        })),
      })
    }
  }
}

const OPEN_PROJECT_MUTATION_KEY = ['openProject'] as const

/** A mutation to open a project. */
export function useOpenProjectMutation() {
  const client = useQueryClient()
  const auth = useAuth()
  const { remoteBackend, localBackend } = useBackends()
  const setProjectAsset = useSetProjectAsset()
  const containerData = useContainerData()

  return useMutation({
    mutationKey: OPEN_PROJECT_MUTATION_KEY,
    networkMode: 'always',
    mutationFn: async ({
      title,
      id,
      type,
      hybrid,
      inBackground = false,
      suppressHybridProjectOpen: _ = false,
      ensoPath,
    }: LaunchedProject & { inBackground?: boolean; suppressHybridProjectOpen?: boolean }) => {
      assert(() => !containerData.closingProjects.has(id))
      const session = auth.session
      invariant(session, 'User is not logged in')
      containerData.openingProjects.set(hybrid?.cloudProjectId ?? id, EnsoPath(ensoPath))
      const backend = type === BackendType.remote ? remoteBackend : localBackend

      invariant(backend != null, 'Backend is null')

      await backend
        .openProject(
          id,
          {
            executeAsync: inBackground,
            cognitoCredentials: {
              accessToken: session.accessToken,
              refreshToken: session.refreshToken,
              clientId: session.clientId,
              expireAt: session.expireAt,
              refreshUrl: session.refreshUrl,
            },
            openHybridProjectParameters: hybrid ?? null,
          },
          title,
        )
        .finally(() => {
          containerData.openingProjects.delete(hybrid?.cloudProjectId ?? id)
        })
    },
    onMutate: ({ type, id, parentId }) => {
      const queryKey = getProjectDetailsQueryKey(id)

      client.setQueryData(queryKey, { state: { type: ProjectState.openInProgress } })
      setProjectAsset(type, id, parentId, (asset) => ({
        ...asset,
        projectState: { ...asset.projectState, type: ProjectState.openInProgress },
      }))
    },
    onSuccess: async (_, { title, hybrid, suppressHybridProjectOpen = false }) => {
      if (hybrid && !suppressHybridProjectOpen) {
        await remoteBackend.setHybridOpened(hybrid.cloudProjectId, title)
      }
    },
    onError: async (_, { type, parentId }) => {
      await client.invalidateQueries({ queryKey: [type, 'listDirectory', parentId] })
    },
    onSettled: async () => {
      await client.invalidateQueries({ queryKey: ['project'] })
    },
    meta: {
      invalidates: [['listDirectory'], ['getAssetDetails']],
      awaitInvalidates: true,
    },
  })
}

/** Mutation to close a project. */
export function useCloseProjectMutation() {
  const { getText } = useText()
  const client = useQueryClient()
  const { remoteBackend, localBackend } = useBackends()
  const uploadFile = useUploadFile(remoteBackend, { updateProgress: false })
  const isHybridPending = ref(false)
  const containerData = useContainerData()
  usePreventNavigation({ message: getText('anUploadIsInProgress'), isEnabled: isHybridPending })

  return useMutation({
    mutationKey: ['closeProject'],
    mutationFn: async ({
      type,
      id,
      title,
      hybrid,
    }: Pick<LaunchedProject, 'hybrid' | 'id' | 'parentId' | 'title' | 'type'>) => {
      const backend = type === BackendType.remote ? remoteBackend : localBackend
      invariant(backend != null, 'Backend is null')
      if (hybrid) {
        await remoteBackend.closeProject(hybrid.cloudProjectId, title)
      }
      return backend.closeProject(id, title)
    },
    onMutate: ({ hybrid, id }) => {
      const queryKey = getProjectDetailsQueryKey(id)
      if (hybrid) {
        isHybridPending.value = true
        containerData.closingProjects.add(hybrid.cloudProjectId)
      } else {
        containerData.closingProjects.add(id)
      }
      void client.cancelQueries({ queryKey })
    },
    onSuccess: async (_, { type, id, parentId, hybrid }) => {
      await client.resetQueries({ queryKey: getProjectDetailsQueryKey(id) })

      if (hybrid) {
        const fileName = 'project_root.enso-project'
        const file = await remoteBackend.getProjectArchive(parentId, fileName)
        await uploadFile([
          {
            fileId: hybrid.cloudProjectId,
            fileName,
            parentDirectoryId: hybrid.cloudParentId,
          },
          file,
        ]).catch((error: unknown) => {
          console.error(error)
        })
        invariant(localBackend != null, 'LocalBackend is null')
        await localBackend
          .deleteAsset(hybrid.parentId, { force: true }, null)
          .catch((error) =>
            console.error('Failed to remove local version of hybrid project', error),
          )
        isHybridPending.value = false
        containerData.closingProjects.delete(hybrid.cloudProjectId)
      } else {
        containerData.closingProjects.delete(id)
      }

      await client.invalidateQueries({ queryKey: getProjectDetailsQueryKey(id) })
      await client.invalidateQueries({ queryKey: [type, 'listDirectory', parentId] })
    },
    onError: async (_, { type, id, parentId, hybrid }) => {
      if (hybrid) {
        const fileName = 'project_root.enso-project'
        const file = await remoteBackend.getProjectArchive(parentId, fileName)
        await uploadFile([
          {
            fileId: hybrid.cloudProjectId,
            fileName,
            parentDirectoryId: hybrid.cloudParentId,
          },
          file,
        ]).catch((error: unknown) => {
          console.error('Could not upload project', error)
        })

        invariant(localBackend != null, 'LocalBackend is null')
        await localBackend
          .deleteAsset(hybrid.parentId, { force: true }, null)
          .catch((error) =>
            console.error('Failed to remove local version of hybrid project', error),
          )
        containerData.closingProjects.delete(hybrid.cloudProjectId)
        isHybridPending.value = false
        await client.invalidateQueries({
          queryKey: getProjectDetailsQueryKey(hybrid.cloudProjectId),
        })
        await client.invalidateQueries({
          queryKey: [BackendType.remote, 'listDirectory', hybrid.cloudParentId],
        })
      } else {
        containerData.closingProjects.delete(id)
      }

      await client.invalidateQueries({ queryKey: getProjectDetailsQueryKey(id) })
      await client.invalidateQueries({ queryKey: [type, 'listDirectory', parentId] })
    },
    meta: {
      invalidates: [['listDirectory'], ['getAssetDetails']],
      awaitInvalidates: true,
    },
  })
}

/** Mutation to rename a project. */
export function useRenameProjectMutation() {
  const containerData = useContainerData()
  const client = useQueryClient()

  return useMutation({
    mutationKey: ['renameProject'],
    mutationFn: ({
      newName,
      project,
      backend,
    }: {
      newName: string
      project: LaunchedProject
      backend: Backend
    }) => {
      const { id, title } = project

      return backend.updateProject(id, { projectName: newName }, title)
    },
    onMutate: async ({ newName, project }) => {
      const queryKey = getProjectDetailsQueryKey(project.id)
      await client.cancelQueries({ queryKey })
      // Optimistically update the project name.
      client.setQueryData<Project>(queryKey, (data) => {
        if (data == null) return undefined
        return { ...data, name: newName }
      })
      return { queryKey }
    },
    onError: (_err, _variables, context) => {
      if (context?.queryKey) {
        const toInvalidate = [['listDirectory'], ['getAssetDetails'], context.queryKey]
        return Promise.all(toInvalidate.map((queryKey) => client.invalidateQueries({ queryKey })))
      }
    },
    onSuccess: (_, { newName, project }) => {
      containerData.updateLaunchedProjects((projects) =>
        projects.map((otherProject) =>
          project.id !== otherProject.id ? otherProject : merge(otherProject, { title: newName }),
        ),
      )
    },
    meta: {
      invalidates: [['listDirectory'], ['project'], ['getAssetDetails']],
      awaitInvalidates: true,
    },
  })
}

/** A callback to open a project. */
function useOpenProject() {
  const client = useQueryClient()
  const containerData = useContainerData()
  const { addLaunchedProject, removeLaunchedProject } = useContainerData()
  const closeAllProjects = useCloseAllProjects()
  const openProjectMutation = useOpenProjectMutation()
  const enableMultitabs = useFeatureFlag('enableMultitabs')

  return async (project: LaunchedProject) => {
    const existingMutation = client.getMutationCache().find({
      mutationKey: ['openProject'],
      predicate: (mutation) => mutation.options.scope?.id === project.id,
    })
    const isOpeningTheSameProject = existingMutation?.state.status === 'pending'

    if (!isOpeningTheSameProject) {
      const queryKey = getProjectDetailsQueryKey(project.id)
      client.setQueryData(queryKey, { state: { type: ProjectState.openInProgress } })

      containerData.openingProjects.set(
        project.hybrid?.cloudProjectId ?? project.id,
        EnsoPath(project.ensoPath),
      )

      if (!enableMultitabs.value) {
        // Since multiple tabs cannot be opened at the same time, the opened projects need to be closed first.
        // The current project is opened as launched above.
        if (containerData.openedProjects.length > 0) {
          await closeAllProjects()
        }
      }

      addLaunchedProject(project)

      void openProjectMutation
        .mutateAsync(project)
        .catch(() => {
          removeLaunchedProject(project.id)
          const newData = client.getQueryData(queryKey)
          // If state has not changed from optimistic state, then:
          if (OPEN_IN_PROGRESS_PROJECT_STATE_SCHEMA.safeParse(newData).success) {
            client.setQueryData(queryKey, { state: { type: ProjectState.closed } })
            void client.invalidateQueries({ queryKey: ['project'] })
          }
        })
        .finally(() => {
          containerData.openingProjects.delete(project.hybrid?.cloudProjectId ?? project.id)
        })

      const openingProjectMutation = client.getMutationCache().find({
        mutationKey: ['openProject'],
        predicate: (mutation) => mutation.state.variables?.id === project.id,
      })
      openingProjectMutation?.setOptions({
        ...openingProjectMutation.options,
        scope: { id: project.id },
      })
    }
  }
}

/** Return a hook to open a project in Hybrid Mode. */
function useOpenHybridProject() {
  const { localBackend, remoteBackend } = useBackends()
  const openProject = useOpenProject()
  const closeProject = useCloseProject()
  const containerData = useContainerData()

  return async (asset: Pick<ProjectAsset, 'ensoPath' | 'id' | 'parentId' | 'title'>) => {
    let launchedProject: LaunchedProject | undefined

    try {
      invariant(localBackend != null, 'Local Backend is null')
      containerData.openingProjects.set(asset.id, asset.ensoPath)
      const projectSessionId = await remoteBackend.setHybridOpenInProgress(asset.id, asset.title)
      const localProject = await remoteBackend.downloadProject(asset.id)
      const cloudProjectDirectoryPath = EnsoPath(
        asset.ensoPath.slice(0, asset.ensoPath.lastIndexOf('/')),
      )

      let project
      for (const parentId of [localProject.parentId, localProject.projectRootId]) {
        const { assets } = await localBackend.listDirectory({
          parentId: parentId,
          filterBy: null,
          labels: null,
          sortExpression: null,
          sortDirection: null,
          from: null,
          pageSize: null,
          recentProjects: false,
        })
        project = assets.filter((item) => item.type === AssetType.project).at(0)
        if (project) {
          break
        }
      }

      invariant(
        project,
        `Downloaded cloud project does not exist in Local Backend (checked path ${extractTypeAndPath(localProject.parentId).path}).`,
      )
      launchedProject = {
        id: project.id,
        title: asset.title,
        parentId: project.parentId,
        ensoPath: asset.ensoPath,
        type: BackendType.local,
        hybrid: {
          cloudProjectId: asset.id,
          cloudProjectSessionId: projectSessionId,
          cloudParentId: asset.parentId,
          parentId: localProject.parentId,
          cloudProjectDirectoryPath,
        },
      }
      await openProject(launchedProject)
    } catch (error) {
      console.error(error)
      await Promise.allSettled([
        closeProject({ ...asset, type: BackendType.remote }),
        ...(launchedProject ? [closeProject(launchedProject)] : []),
      ])
    } finally {
      containerData.openingProjects.delete(asset.id)
    }
  }
}

/** Whether the user can run projects. */
function useCanRunProjects() {
  const auth = useAuth()
  const backends = useBackends()
  const enableCloudExecution = useFeatureFlag('enableCloudExecution')
  const hasLocalBackend = computed(() => backends.localBackend != null)

  return {
    // All projects can be run locally.
    // Local projects: Open normally
    // Cloud projects: Open in Hybrid
    locally: {
      [BackendType.local]: hasLocalBackend,
      [BackendType.remote]: hasLocalBackend,
    },
    // Local projects can be run natively; only Team plans and above have access to Cloud execution.
    // Local projects: Open normally
    // Cloud projects: Open in Cloud VM
    natively: {
      [BackendType.local]: hasLocalBackend,
      [BackendType.remote]: computed(
        () =>
          enableCloudExecution.value &&
          (auth.session?.user?.plan === Plan.team || auth.session?.user?.plan === Plan.enterprise),
      ),
    },
  }
}

/** Return a function to reopen a previously opened project that has since been closed. */
export function useReopenProject(openProjectMutation: ReturnType<typeof useOpenProjectMutation>) {
  const { remoteBackend } = useBackends()

  return async (project: LaunchedProject & { readonly suppressHybridProjectOpen?: boolean }) => {
    if (project.hybrid && project.suppressHybridProjectOpen !== true) {
      await remoteBackend.setHybridOpenInProgress(project.hybrid.cloudProjectId, project.title)
    }
    await openProjectMutation.mutateAsync(project)
  }
}

/** Return a function to open a project natively - Cloud mode for cloud projects, Local mode for local projects. */
export function useOpenProjectNatively() {
  const canRunProjects = useCanRunProjects()
  const openProject = useOpenProject()

  return async (
    asset: Pick<ProjectAsset, 'ensoPath' | 'id' | 'parentId' | 'title'>,
    backendType: BackendType,
  ) => {
    if (!canRunProjects.natively[backendType]) {
      return
    }
    await openProject({ ...asset, type: backendType })
  }
}

/** Return a function to open a project locally - meaning Hybrid Mode is used for Cloud projects. */
export function useOpenProjectLocally() {
  const openProject = useOpenProject()
  const canRunProjects = useCanRunProjects()
  const openHybridProject = useOpenHybridProject()

  return async (
    asset: Pick<ProjectAsset, 'ensoPath' | 'id' | 'parentId' | 'title'>,
    backendType: BackendType,
  ) => {
    if (!canRunProjects.locally[backendType]) {
      return
    }
    const isCloud = backendType === BackendType.remote
    if (isCloud) {
      await openHybridProject(asset)
    } else {
      await openProject({ ...asset, type: backendType })
    }
  }
}

/** A function to close a project. */
export function useCloseProject() {
  const client = useQueryClient()
  const closeProjectMutation = useCloseProjectMutation()
  const containerData = useContainerData()

  return async (
    project: Pick<LaunchedProject, 'hybrid' | 'id' | 'parentId' | 'title' | 'type'>,
  ) => {
    client
      .getMutationCache()
      .findAll({
        mutationKey: ['openProject'],
        predicate: (mutation) => mutation.options.scope?.id === project.id,
      })
      .forEach((mutation) => {
        mutation.setOptions({ ...mutation.options, retry: false })
        mutation.destroy()
      })

    const promise = closeProjectMutation.mutateAsync(project)

    client
      .getMutationCache()
      .findAll({
        mutationKey: ['closeProject'],
        predicate: (mutation) => mutation.state.variables?.id === project.id,
      })
      .forEach((mutation) => {
        mutation.setOptions({ ...mutation.options, scope: { id: project.id } })
      })

    containerData.removeLaunchedProject(project.id)

    await promise
  }
}

/** A function to close all projects. */
export function useCloseAllProjects() {
  const closeProject = useCloseProject()
  const containerData = useContainerData()
  const { remoteBackend, localBackend } = useBackends()
  const ensureQueryData = useVueEnsureQueryData()

  return async () => {
    await Promise.all(
      containerData.openedProjects.map(async (project) => {
        if (project.state === 'launched') {
          const backend =
            project.type === BackendType.remote || project.hybrid != null ?
              remoteBackend
            : localBackend
          invariant(backend != null, 'Backend must not be async null')
          const projectDetails = await ensureQueryData(
            createGetProjectDetailsQuery({
              assetId: project.hybrid != null ? project.hybrid.cloudProjectId : project.id,
              backend,
            }),
          )
          if (IS_OPENING_OR_OPENED[projectDetails.state.type]) {
            await closeProject(project)
          } else {
            containerData.removeLaunchedProject(project.id)
          }
        } else {
          containerData.openingProjects.delete(project.id)
        }
      }),
    )
  }
}
