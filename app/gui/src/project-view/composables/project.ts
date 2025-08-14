import { backendMutationOptions } from '#/hooks/backendHooks'
import { createGetProjectDetailsQuery } from '#/hooks/projectHooks'
import { setModal } from '#/providers/ModalProvider'
import { projectsStore } from '#/providers/ProjectsProvider/hooks'
import Backend, {
  AssetType,
  BackendType,
  EnsoPath,
  IS_OPENING_OR_OPENED,
  Plan,
  ProjectState,
  S3_CHUNK_SIZE_BYTES,
  UploadedAsset,
  UploadFileRequestParams,
  type AnyAsset,
  type AssetId,
  type DirectoryId,
  type ProjectAsset,
} from '#/services/Backend'
import { MergeValuesOfObjectUnion } from '#/utilities/object'
import { ConfirmCloseModal } from '#/utilities/preventNavigation'
import { useVueEnsureQueryData } from '#/utilities/tanstackQuery'
import { useAuth } from '$/providers/auth'
import { useBackends } from '$/providers/backends'
import { LaunchedProject, useContainerData } from '$/providers/container'
import { useFeatureFlag } from '$/providers/featureFlags'
import { useText } from '$/providers/text'
import {
  QueryClient,
  QueryKey,
  useMutation,
  UseMutationReturnType,
  useQueryClient,
} from '@tanstack/vue-query'
import { isOnElectron } from 'enso-common/src/detect'
import { uniqueString } from 'enso-common/src/utilities/uniqueString'
import { noop } from 'motion-v'
import { createElement } from 'react'
import invariant from 'tiny-invariant'
import { computed, MaybeRef, Ref, ref, toValue, watchEffect } from 'vue'
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
function usePreventNavigation(options: PreventNavigationOptions) {
  const { isEnabled = true, message } = options

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
function useUploadFileMutation(
  backend: Backend,
  options: UploadFileMutationOptions = {},
): UploadFileMutationResult {
  const queryClient = useQueryClient()
  const { getText } = useText()
  const fileChunkUploadPoolSize = useFeatureFlag('fileChunkUploadPoolSize')
  const {
    retries = 3,
    chunkRetries = retries,
    endRetries = retries,
    updateProgress = true,
    onError = (error) => {
      console.error(error)
    },
  } = options
  const setProgress: typeof setUploadingFileProgress =
    updateProgress ? setUploadingFileProgress : noop
  const uploadFileStartMutation = useMutation(backendMutationOptions(backend, 'uploadFileStart'))
  const variables = ref<[params: UploadFileRequestParams, file: File]>()
  const sentBytes = ref(0)
  const totalBytes = ref(0)
  const uploadFileChunkMutation = useMutation(
    backendMutationOptions(backend, 'uploadFileChunk', {
      retry: chunkRetries,
      meta: { pool: { id: 'uploadFileChunk', parallelism: fileChunkUploadPoolSize.value } },
    }),
  )
  const uploadFileEndMutation = useMutation(
    backendMutationOptions(backend, 'uploadFileEnd', { retry: endRetries }),
  )
  const mutateAsync = async ([body, file]: [body: UploadFileRequestParams, file: File]) => {
    const progressId = uniqueString()
    variables.value = [body, file]
    const fileSizeBytes = file.size
    const beginProgress: UploadFileMutationProgress = {
      event: 'begin',
      sentBytes: 0,
      totalBytes: fileSizeBytes,
    }
    options.onBegin?.(beginProgress)
    setProgress(queryClient, progressId, beginProgress)
    sentBytes.value = 0
    totalBytes.value = fileSizeBytes
    try {
      const { sourcePath, uploadId, presignedUrls } = await uploadFileStartMutation.mutateAsync([
        body,
        file,
      ])
      let completedChunkCount = 0
      const parts = await Promise.all(
        presignedUrls.map((url, i) =>
          uploadFileChunkMutation.mutateAsync([url, file, i]).then((part) => {
            // This cannot be the `onSuccess` callback in `mutateAsync` because then it would not run
            // if the component is unmounted beforehand (which seems to be the case?).
            completedChunkCount += 1
            const newSentBytes = Math.min(completedChunkCount * S3_CHUNK_SIZE_BYTES, fileSizeBytes)
            sentBytes.value = newSentBytes
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
      const result = await uploadFileEndMutation.mutateAsync([
        {
          parentDirectoryId: body.parentDirectoryId,
          parts,
          sourcePath: sourcePath,
          uploadId: uploadId,
          assetId: body.fileId,
          fileName: body.fileName,
        },
      ])
      sentBytes.value = fileSizeBytes
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
    }
  }

  const mutate = ([params, file]: [params: UploadFileRequestParams, file: File]) => {
    void mutateAsync([params, file])
  }

  const reset = () => {
    uploadFileStartMutation.reset()
    uploadFileChunkMutation.reset()
    uploadFileEndMutation.reset()
  }

  const submittedAt = uploadFileStartMutation.submittedAt

  const isError = computed(
    () =>
      uploadFileStartMutation.isError.value ||
      uploadFileChunkMutation.isError.value ||
      uploadFileEndMutation.isError.value,
  )
  const isSuccess = uploadFileEndMutation.isSuccess
  const isPending = computed(
    () =>
      uploadFileStartMutation.isPending.value ||
      uploadFileChunkMutation.isPending.value ||
      uploadFileEndMutation.isPending.value,
  )
  const isIdle = computed(
    () =>
      uploadFileStartMutation.isIdle.value &&
      uploadFileChunkMutation.isIdle.value &&
      uploadFileEndMutation.isIdle.value,
  )

  usePreventNavigation({ message: getText('anUploadIsInProgress'), isEnabled: isPending })

  const result: MergeValuesOfObjectUnion<UploadFileMutationResult> = {
    sentBytes,
    totalBytes,
    variables,
    mutate,
    mutateAsync,
    context: uploadFileEndMutation.context,
    data: uploadFileEndMutation.data,
    failureCount: computed(
      () =>
        uploadFileEndMutation.failureCount.value +
        uploadFileChunkMutation.failureCount.value +
        uploadFileStartMutation.failureCount.value,
    ),
    failureReason: computed(
      () =>
        uploadFileEndMutation.failureReason.value ??
        uploadFileChunkMutation.failureReason.value ??
        uploadFileStartMutation.failureReason.value,
    ),
    isError,
    error: computed(
      () =>
        uploadFileEndMutation.error.value ??
        uploadFileChunkMutation.error.value ??
        uploadFileStartMutation.error.value,
    ),
    isPaused: computed(
      () =>
        uploadFileStartMutation.isPaused.value ||
        uploadFileChunkMutation.isPaused.value ||
        uploadFileEndMutation.isPaused.value,
    ),
    isPending,
    isSuccess,
    isIdle,
    status: computed(() =>
      isPending.value ? 'pending'
      : isIdle.value ? 'idle'
      : isSuccess.value ? 'success'
      : isError.value ? 'error'
      : 'error',
    ),
    reset,
    submittedAt,
  }
  // This is UNSAFE. Care must be taken to ensire all state is merged properly.

  return result as UploadFileMutationResult
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
    const listDirectoryQuery = queryClient.getQueryCache().find<readonly AnyAsset[] | undefined>({
      queryKey: [backendType, 'listDirectory', parentId],
      exact: false,
    })

    if (listDirectoryQuery?.state.data) {
      listDirectoryQuery.setData(
        listDirectoryQuery.state.data.map((child) =>
          child.id === assetId && child.type === AssetType.project ? transform(child) : child,
        ),
      )
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
  const addOpeningProject = projectsStore.getState().addOpeningProject
  const removeOpeningProject = projectsStore.getState().removeOpeningProject

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
    }: LaunchedProject & { inBackground?: boolean; suppressHybridProjectOpen?: boolean }) => {
      const session = auth.session
      invariant(session, 'User is not logged in')
      addOpeningProject(hybrid?.cloudProjectId ?? id)
      const backend = type === BackendType.remote ? remoteBackend : localBackend

      invariant(backend != null, 'Backend is null')

      const openHybridProjectParameters = hybrid ? { ...hybrid } : null
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
            openHybridProjectParameters,
          },
          title,
        )
        .finally(() => {
          removeOpeningProject(hybrid?.cloudProjectId ?? id)
        })
    },
    onMutate: ({ type, id, parentId }) => {
      const queryKey = createGetProjectDetailsQuery.getQueryKey(id)

      client.setQueryData(queryKey, { state: { type: ProjectState.openInProgress } })
      setProjectAsset(type, id, parentId, (asset) => ({
        ...asset,
        projectState: { ...asset.projectState, type: ProjectState.openInProgress },
      }))
    },
    onSuccess: async (_, { title, hybrid, suppressHybridProjectOpen = false }) => {
      await client.cancelQueries({ queryKey: ['project'] })
      if (hybrid && !suppressHybridProjectOpen) {
        await remoteBackend.setHybridOpened(hybrid.cloudProjectId, title)
      }
    },
    onError: async (_, { type, parentId }) => {
      await client.invalidateQueries({ queryKey: ['project'] })
      await client.invalidateQueries({ queryKey: [type, 'listDirectory', parentId] })
    },
    meta: {
      invalidates: [['listDirectory'], ['project'], ['getAssetDetails']],
      awaitInvalidates: true,
    },
  })
}

/** A callback to open a project. */
export function useOpenProject() {
  const client = useQueryClient()
  const containerData = useContainerData()
  const addOpeningProject = projectsStore.getState().addOpeningProject
  const removeOpeningProject = projectsStore.getState().removeOpeningProject
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
      const queryKey = createGetProjectDetailsQuery.getQueryKey(project.id)
      client.setQueryData(queryKey, { state: { type: ProjectState.openInProgress } })

      addOpeningProject(project.hybrid?.cloudProjectId ?? project.id)

      if (!enableMultitabs) {
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
          removeOpeningProject(project.hybrid?.cloudProjectId ?? project.id)
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

/** Mutation to close a project. */
export function useCloseProjectMutation() {
  const client = useQueryClient()
  const { remoteBackend, localBackend } = useBackends()
  const setProjectAsset = useSetProjectAsset()
  const uploadFileMutation = useUploadFileMutation(remoteBackend, { updateProgress: false })

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
    onMutate: ({ type, id, parentId }) => {
      const queryKey = createGetProjectDetailsQuery.getQueryKey(id)

      client.setQueryData(queryKey, { state: { type: ProjectState.closing } })
      setProjectAsset(type, id, parentId, (asset) => ({
        ...asset,
        projectState: { ...asset.projectState, type: ProjectState.closing },
      }))

      void client.cancelQueries({ queryKey })
    },
    onSuccess: async (_, { type, id, parentId, hybrid }) => {
      await client.resetQueries({ queryKey: createGetProjectDetailsQuery.getQueryKey(id) })
      setProjectAsset(type, id, parentId, (asset) => ({
        ...asset,
        projectState: { ...asset.projectState, type: ProjectState.closed },
      }))

      if (hybrid) {
        const fileName = 'project_root.enso-project'
        const file = await remoteBackend.getProjectArchive(parentId, fileName)
        await uploadFileMutation
          .mutateAsync([
            {
              fileId: hybrid.cloudProjectId,
              fileName,
              parentDirectoryId: hybrid.cloudParentId,
            },
            file,
          ])
          .catch((error: unknown) => {
            console.error(error)
          })

        invariant(localBackend != null, 'LocalBackend is null')
        await localBackend.deleteAsset(hybrid.parentId, { force: true }, null)
      }

      await client.invalidateQueries({ queryKey: createGetProjectDetailsQuery.getQueryKey(id) })
      await client.invalidateQueries({ queryKey: [type, 'listDirectory', parentId] })
    },
    onError: async (_, { type, id, parentId, hybrid }) => {
      if (hybrid) {
        const fileName = 'project_root.enso-project'
        const file = await remoteBackend.getProjectArchive(parentId, fileName)
        await uploadFileMutation
          .mutateAsync([
            {
              fileId: hybrid.cloudProjectId,
              fileName,
              parentDirectoryId: hybrid.cloudParentId,
            },
            file,
          ])
          .catch((error: unknown) => {
            console.error(error)
          })

        invariant(localBackend != null, 'LocalBackend is null')
        await localBackend.deleteAsset(hybrid.parentId, { force: true }, null)
      }

      await client.invalidateQueries({ queryKey: createGetProjectDetailsQuery.getQueryKey(id) })
      await client.invalidateQueries({ queryKey: [type, 'listDirectory', parentId] })
    },
    meta: {
      invalidates: [['listDirectory'], ['getAssetDetails']],
      awaitInvalidates: true,
    },
  })
}

/** A function to close a project. */
export function useCloseProject() {
  const client = useQueryClient()
  const closeProjectMutation = useCloseProjectMutation()
  const { removeLaunchedProject } = useContainerData()

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
        // This is unsafe, but we cannot do anything about it.

        predicate: (mutation) => mutation.state.variables?.id === project.id,
      })
      .forEach((mutation) => {
        mutation.setOptions({ ...mutation.options, scope: { id: project.id } })
      })

    removeLaunchedProject(project.id)

    await promise
  }
}

/** A function to close all projects. */
export function useCloseAllProjects() {
  const closeProject = useCloseProject()
  const containerData = useContainerData()
  const { removeLaunchedProject } = containerData
  const { remoteBackend, localBackend } = useBackends()
  const ensureQueryData = useVueEnsureQueryData()

  return async () => {
    const launchedProjects = containerData.openedProjects

    await Promise.all(
      launchedProjects.map(async (project) => {
        const backend =
          project.type === BackendType.remote || project.hybrid != null ?
            remoteBackend
          : localBackend
        invariant(backend != null, 'Backend must not be null')
        const projectDetails = await ensureQueryData(
          createGetProjectDetailsQuery({
            assetId: project.hybrid != null ? project.hybrid.cloudProjectId : project.id,
            backend,
          }),
        )
        if (IS_OPENING_OR_OPENED[projectDetails.state.type]) {
          await closeProject(project)
        } else {
          removeLaunchedProject(project.id)
        }
      }),
    )
  }
}

/** Return a hook to open a project in Hybrid Mode. */
function useOpenHybridProject() {
  const { localBackend, remoteBackend } = useBackends()
  const openProject = useOpenProject()
  const closeProject = useCloseProject()
  const addOpeningProject = projectsStore.getState().addOpeningProject
  const removeOpeningProject = projectsStore.getState().removeOpeningProject

  return async (asset: Pick<ProjectAsset, 'ensoPath' | 'id' | 'parentId' | 'title'>) => {
    let launchedProject: LaunchedProject | undefined

    try {
      invariant(localBackend != null, 'Local Backend is null')
      addOpeningProject(asset.id)
      const projectSessionId = await remoteBackend.setHybridOpenInProgress(asset.id, asset.title)
      const localProject = await remoteBackend.downloadProject(asset.id)
      const cloudProjectDirectoryPath = EnsoPath(
        asset.ensoPath.slice(0, asset.ensoPath.lastIndexOf('/')),
      )

      let project
      for (const parentId of [localProject.parentId, localProject.projectRootId]) {
        const assets = await localBackend.listDirectory({
          parentId,
          filterBy: null,
          labels: null,
          recentProjects: false,
        })
        project = assets.filter((item) => item.type === AssetType.project).at(0)
        if (project) {
          break
        }
      }

      removeOpeningProject(asset.id)
      invariant(project, 'Downloaded cloud project does not exist in Local Backend.')
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
      removeOpeningProject(asset.id)
      console.error(error)
      await Promise.allSettled([
        closeProject({ ...asset, type: BackendType.remote }),
        ...(launchedProject ? [closeProject(launchedProject)] : []),
      ])
    }
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
