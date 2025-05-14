/** @file Mutations related to project management. */
import * as reactQuery from '@tanstack/react-query'
import invariant from 'tiny-invariant'

import { merge } from 'enso-common/src/utilities/data/object'

import * as eventCallbacks from '#/hooks/eventCallbackHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import {
  useAddLaunchedProject,
  useProjectsStore,
  useRemoveLaunchedProject,
  useSetPage,
  useUpdateLaunchedProjects,
  type LaunchedProject,
  type LaunchedProjectId,
} from '#/providers/ProjectsProvider'

import { useUploadFileMutation } from '#/hooks/backendUploadFilesHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import { useFeatureFlag } from '#/providers/FeatureFlagsProvider'
import { useAddOpeningProject, useRemoveOpeningProject } from '#/providers/ProjectsProvider/hooks'
import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'
import { z } from 'zod'
import { useEnsureQueryData, useMutationCallback } from '../utilities/tanstackQuery'

/** Default interval for refetching project status when the project is opened. */
const OPENED_INTERVAL_MS = 30_000
/**
 * Interval when we open a cloud project.
 * Since opening a cloud project is a long operation, we want to check the status less often.
 */
const CLOUD_OPENING_INTERVAL_MS = 2_500
/**
 * Interval when we open a local project or when we want to sync the project status as soon as
 * possible.
 */
const LOCAL_OPENING_INTERVAL_MS = 100

const DEFAULT_INTERVAL_MS = 120_000

/** Options for {@link createGetProjectDetailsQuery}. */
export interface CreateOpenedProjectQueryOptions {
  readonly assetId: backendModule.Asset<backendModule.AssetType.project>['id']
  readonly backend: Backend
}

/** Whether the user can open projects. */
export function useCanOpenProjects() {
  return useFeatureFlag('enableCloudExecution')
}

/** Return a function to update a project asset in the TanStack Query cache. */
function useSetProjectAsset() {
  const queryClient = reactQuery.useQueryClient()
  return eventCallbacks.useEventCallback(
    (
      backendType: backendModule.BackendType,
      assetId: backendModule.AssetId,
      parentId: backendModule.DirectoryId,
      transform: (asset: backendModule.ProjectAsset) => backendModule.ProjectAsset,
    ) => {
      const listDirectoryQuery = queryClient
        .getQueryCache()
        .find<readonly backendModule.AnyAsset<backendModule.AssetType>[] | undefined>({
          queryKey: [backendType, 'listDirectory', parentId],
          exact: false,
        })

      if (listDirectoryQuery?.state.data) {
        listDirectoryQuery.setData(
          listDirectoryQuery.state.data.map((child) =>
            child.id === assetId && child.type === backendModule.AssetType.project ?
              transform(child)
            : child,
          ),
        )
      }
    },
  )
}

export const OPENING_PROJECT_STATES = new Set([
  backendModule.ProjectState.provisioned,
  backendModule.ProjectState.scheduled,
  backendModule.ProjectState.openInProgress,
  backendModule.ProjectState.closing,
])
export const OPENED_PROJECT_STATES = new Set([backendModule.ProjectState.opened])
export const CLOSED_PROJECT_STATES = new Set([backendModule.ProjectState.closed])
export const CLOSING_PROJECT_STATES = new Set([backendModule.ProjectState.closing])
export const STATIC_PROJECT_STATES = new Set([
  backendModule.ProjectState.opened,
  backendModule.ProjectState.closed,
])
export const CREATED_PROJECT_STATES = new Set([
  backendModule.ProjectState.created,
  backendModule.ProjectState.new,
])
export const BUSY_PROJECT_STATES = new Set([
  ...Array.from(OPENING_PROJECT_STATES),
  ...Array.from(CLOSING_PROJECT_STATES),
  backendModule.ProjectState.opened,
])

/** Stale time for local projects, set to 10 seconds. */
// eslint-disable-next-line @typescript-eslint/no-magic-numbers
export const LOCAL_PROJECT_OPEN_TIMEOUT_MS = 10 * 1_000
/** Stale time for cloud projects, set to 5 minutes. */
// eslint-disable-next-line @typescript-eslint/no-magic-numbers
export const CLOUD_PROJECT_OPEN_TIMEOUT_MS = 5 * 60 * 1_000

/**
 * Get the timeout based on the backend type.
 * @param backendType - The backend type.
 * @throws If the backend type is not supported.
 * @returns The timeout in milliseconds.
 */
export function getTimeoutBasedOnTheBackendType(backendType: backendModule.BackendType) {
  switch (backendType) {
    case backendModule.BackendType.local: {
      return LOCAL_PROJECT_OPEN_TIMEOUT_MS
    }
    case backendModule.BackendType.remote: {
      return CLOUD_PROJECT_OPEN_TIMEOUT_MS
    }

    default: {
      throw new Error('Unsupported backend type')
    }
  }
}

/** Project status query.  */
export function createGetProjectDetailsQuery(options: CreateOpenedProjectQueryOptions) {
  const { assetId, backend } = options

  const isLocal = backend.type === backendModule.BackendType.local

  return reactQuery.queryOptions({
    queryKey: createGetProjectDetailsQuery.getQueryKey(assetId),
    queryFn: () => backend.getProjectDetails(assetId),
    refetchIntervalInBackground: true,
    refetchOnWindowFocus: true,
    refetchOnMount: true,
    networkMode: backend.type === backendModule.BackendType.remote ? 'online' : 'always',
    meta: { persist: false },
    refetchInterval: (query): number | false => {
      const { state } = query

      const staticStates = STATIC_PROJECT_STATES

      const openingStates = OPENING_PROJECT_STATES

      const createdStates = CREATED_PROJECT_STATES

      const closingStates = CLOSING_PROJECT_STATES

      if (state.status === 'error') {
        return false
      }

      if (state.data == null) {
        return false
      }

      const currentState = state.data.state.type

      if (isLocal) {
        if (createdStates.has(currentState)) {
          return LOCAL_OPENING_INTERVAL_MS
        }

        if (staticStates.has(state.data.state.type)) {
          return OPENED_INTERVAL_MS
        }

        if (openingStates.has(state.data.state.type)) {
          return LOCAL_OPENING_INTERVAL_MS
        }

        if (closingStates.has(state.data.state.type)) {
          return LOCAL_OPENING_INTERVAL_MS
        }
      }

      if (createdStates.has(currentState)) {
        return CLOUD_OPENING_INTERVAL_MS
      }

      // Cloud project
      if (staticStates.has(state.data.state.type)) {
        return OPENED_INTERVAL_MS
      }
      if (openingStates.has(state.data.state.type)) {
        return CLOUD_OPENING_INTERVAL_MS
      }

      if (closingStates.has(state.data.state.type)) {
        return CLOUD_OPENING_INTERVAL_MS
      }

      return DEFAULT_INTERVAL_MS
    },
  })
}
createGetProjectDetailsQuery.getQueryKey = (id: LaunchedProjectId) => ['project', id] as const

const OPEN_PROJECT_MUTATION_KEY = ['openProject'] as const

/** A mutation to open a project. */
export function useOpenProjectMutation() {
  const client = reactQuery.useQueryClient()
  const session = authProvider.useFullUserSession()
  const remoteBackend = backendProvider.useRemoteBackend()
  const localBackend = backendProvider.useLocalBackend()
  const setProjectAsset = useSetProjectAsset()
  const addOpeningProject = useAddOpeningProject()
  const removeOpeningProject = useRemoveOpeningProject()

  return reactQuery.useMutation({
    mutationKey: OPEN_PROJECT_MUTATION_KEY,
    networkMode: 'always',
    mutationFn: async ({
      title,
      id,
      type,
      parentId,
      hybrid,
      inBackground = false,
      suppressHybridProjectOpen: _ = false,
    }: LaunchedProject & { inBackground?: boolean; suppressHybridProjectOpen?: boolean }) => {
      addOpeningProject(hybrid?.cloudProjectId ?? id)
      const backend = type === backendModule.BackendType.remote ? remoteBackend : localBackend

      invariant(backend != null, 'Backend is null')
      const cloudProjectDirectoryPath = hybrid ? hybrid.cloudProjectDirectoryPath : null

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
            cloudProjectDirectoryPath,
            parentId,
          },
          title,
        )
        .finally(() => {
          removeOpeningProject(hybrid?.cloudProjectId ?? id)
        })
    },
    onMutate: ({ type, id, parentId }) => {
      const queryKey = createGetProjectDetailsQuery.getQueryKey(id)

      client.setQueryData(queryKey, { state: { type: backendModule.ProjectState.openInProgress } })
      setProjectAsset(type, id, parentId, (asset) => ({
        ...asset,
        projectState: { ...asset.projectState, type: backendModule.ProjectState.openInProgress },
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

/** Mutation to close a project. */
export function useCloseProjectMutation() {
  const client = reactQuery.useQueryClient()
  const remoteBackend = backendProvider.useRemoteBackend()
  const localBackend = backendProvider.useLocalBackend()
  const setProjectAsset = useSetProjectAsset()
  const uploadFileMutation = useUploadFileMutation(remoteBackend, { updateProgress: false })
  const toastAndLog = useToastAndLog()

  return useMutationCallback({
    mutationKey: ['closeProject'],
    mutationFn: async ({ type, id, title, hybrid }: LaunchedProject) => {
      const backend = type === backendModule.BackendType.remote ? remoteBackend : localBackend

      invariant(backend != null, 'Backend is null')

      if (hybrid) {
        await remoteBackend.closeProject(hybrid.cloudProjectId, title)
      }

      return backend.closeProject(id, title)
    },
    onMutate: ({ type, id, parentId }) => {
      const queryKey = createGetProjectDetailsQuery.getQueryKey(id)

      client.setQueryData(queryKey, { state: { type: backendModule.ProjectState.closing } })
      setProjectAsset(type, id, parentId, (asset) => ({
        ...asset,
        projectState: { ...asset.projectState, type: backendModule.ProjectState.closing },
      }))

      void client.cancelQueries({ queryKey })
    },
    onSuccess: async (_, { type, id, parentId, hybrid }) => {
      await client.resetQueries({ queryKey: createGetProjectDetailsQuery.getQueryKey(id) })
      setProjectAsset(type, id, parentId, (asset) => ({
        ...asset,
        projectState: { ...asset.projectState, type: backendModule.ProjectState.closed },
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
          .catch((error) => {
            toastAndLog('uploadProjectError', error)
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
          .catch((error) => {
            toastAndLog('uploadProjectError', error)
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

/** Mutation to rename a project. */
export function useRenameProjectMutation() {
  const updateLaunchedProjects = useUpdateLaunchedProjects()

  return useMutationCallback({
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
    onSuccess: (_, { newName, project }) => {
      updateLaunchedProjects((projects) =>
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

const OPEN_IN_PROGRESS_PROJECT_STATE_SCHEMA = z.object({
  state: z.object({ type: z.literal(backendModule.ProjectState.openInProgress) }),
})

/** A callback to open a project. */
function useOpenProject() {
  const client = reactQuery.useQueryClient()
  const canOpenProjects = useCanOpenProjects()
  const projectsStore = useProjectsStore()
  const addOpeningProject = useAddOpeningProject()
  const removeOpeningProject = useRemoveOpeningProject()
  const addLaunchedProject = useAddLaunchedProject()
  const removeLaunchedProject = useRemoveLaunchedProject()
  const closeAllProjects = useCloseAllProjects()
  const openProjectMutation = useOpenProjectMutation()

  const enableMultitabs = useFeatureFlag('enableMultitabs')

  return eventCallbacks.useEventCallback(async (project: LaunchedProject) => {
    if (!canOpenProjects) {
      return
    }

    const existingMutation = client.getMutationCache().find({
      mutationKey: ['openProject'],
      predicate: (mutation) => mutation.options.scope?.id === project.id,
    })
    const isOpeningTheSameProject = existingMutation?.state.status === 'pending'

    if (!isOpeningTheSameProject) {
      const queryKey = createGetProjectDetailsQuery.getQueryKey(project.id)
      client.setQueryData(queryKey, { state: { type: backendModule.ProjectState.openInProgress } })

      addOpeningProject(project.hybrid?.cloudProjectId ?? project.id)

      if (!enableMultitabs) {
        // Since multiple tabs cannot be opened at the same time, the opened projects need to be closed first.
        // The current project is opened as launched above.
        if (projectsStore.getState().launchedProjects.length > 0) {
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
            client.setQueryData(queryKey, { state: { type: backendModule.ProjectState.closed } })
            void client.invalidateQueries({ queryKey: ['project'] })
          }
        })
        .finally(() => {
          removeOpeningProject(project.hybrid?.cloudProjectId ?? project.id)
        })

      const openingProjectMutation = client.getMutationCache().find({
        mutationKey: ['openProject'],
        // This is unsafe, but we can't do anything about it.
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
        predicate: (mutation) => mutation.state.variables?.id === project.id,
      })
      openingProjectMutation?.setOptions({
        ...openingProjectMutation.options,
        scope: { id: project.id },
      })
    }
  })
}

/** Return a hook to open a project in Hybrid Mode. */
export function useOpenHybridProject() {
  const localBackend = backendProvider.useLocalBackend()
  const remoteBackend = backendProvider.useRemoteBackend()
  const toastAndLog = useToastAndLog()
  const openProject = useOpenProject()
  const closeProject = useCloseProject()
  const addOpeningProject = useAddOpeningProject()
  const removeOpeningProject = useRemoveOpeningProject()

  return eventCallbacks.useEventCallback(
    async (asset: Pick<backendModule.ProjectAsset, 'ensoPath' | 'id' | 'parentId' | 'title'>) => {
      try {
        invariant(localBackend != null, 'Local Backend is null')
        addOpeningProject(asset.id)
        await remoteBackend.setHybridOpenInProgress(asset.id, asset.title)
        const localProject = await remoteBackend.downloadProject(asset.id)
        invariant(asset.ensoPath, 'Enso path is not defined')
        const cloudProjectDirectoryPath = asset.ensoPath.slice(0, asset.ensoPath.lastIndexOf('/'))

        let project
        for (const parentId of [localProject.targetId, localProject.parentId]) {
          const assets = await localBackend.listDirectory({
            parentId: parentId,
            filterBy: null,
            labels: null,
            recentProjects: false,
          })
          project = assets.filter((item) => item.type === backendModule.AssetType.project).at(0)
          if (project) {
            break
          }
        }

        removeOpeningProject(asset.id)
        invariant(project, 'Downloaded cloud project does not exist in `localProject`.')
        await openProject({
          id: project.id,
          title: asset.title,
          parentId: project.parentId,
          type: backendModule.BackendType.local,
          hybrid: {
            cloudProjectId: asset.id,
            cloudParentId: asset.parentId,
            parentId: localProject.parentId,
            cloudProjectDirectoryPath,
          },
        })
      } catch (error) {
        removeOpeningProject(asset.id)
        toastAndLog('openProjectError', error, asset.title)
        await closeProject({ ...asset, type: backendModule.BackendType.local })
      }
    },
  )
}

/** Return a function to reopen a previously opened project that has since been closed. */
export function useReopenProject(openProjectMutation: ReturnType<typeof useOpenProjectMutation>) {
  const remoteBackend = backendProvider.useRemoteBackend()

  return eventCallbacks.useEventCallback(
    async (project: LaunchedProject & { readonly suppressHybridProjectOpen?: boolean }) => {
      if (project.hybrid && project.suppressHybridProjectOpen !== true) {
        await remoteBackend.setHybridOpenInProgress(project.hybrid.cloudProjectId, project.title)
      }
      await openProjectMutation.mutateAsync(project)
    },
  )
}

/** Return a function to open a project natively - Cloud mode for cloud projects, Local mode for local projects. */
export function useOpenProjectNatively() {
  const openProject = useOpenProject()

  return eventCallbacks.useEventCallback(
    async (
      asset: Pick<backendModule.ProjectAsset, 'id' | 'parentId' | 'title'>,
      backendType: backendModule.BackendType,
    ) => {
      await openProject({ ...asset, type: backendType })
    },
  )
}

/** Return a function to open a project locally - meaning Hybrid Mode is used for Cloud projects. */
export function useOpenProjectLocally() {
  const openProject = useOpenProject()
  const enableHybridExecution = useFeatureFlag('enableHybridExecution')
  const openHybridProject = useOpenHybridProject()

  return eventCallbacks.useEventCallback(
    async (
      asset: Pick<backendModule.ProjectAsset, 'ensoPath' | 'id' | 'parentId' | 'title'>,
      backendType: backendModule.BackendType,
    ) => {
      const isCloud = backendType === backendModule.BackendType.remote
      if (isCloud && enableHybridExecution) {
        await openHybridProject(asset)
      } else {
        await openProject({ ...asset, type: backendType })
      }
    },
  )
}

/** A function to open the editor. */
export function useOpenEditor() {
  const setPage = useSetPage()
  return eventCallbacks.useEventCallback((projectId: LaunchedProjectId) => {
    setPage(projectId)
  })
}

/** A function to close a project. */
export function useCloseProject() {
  const client = reactQuery.useQueryClient()
  const closeProjectMutation = useCloseProjectMutation()
  const removeLaunchedProject = useRemoveLaunchedProject()
  const setPage = useSetPage()
  const projectsStore = useProjectsStore()

  return eventCallbacks.useEventCallback(async (project: LaunchedProject) => {
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

    const promise = closeProjectMutation(project)

    client
      .getMutationCache()
      .findAll({
        mutationKey: ['closeProject'],
        // This is unsafe, but we cannot do anything about it.
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
        predicate: (mutation) => mutation.state.variables?.id === project.id,
      })
      .forEach((mutation) => {
        mutation.setOptions({ ...mutation.options, scope: { id: project.id } })
      })

    removeLaunchedProject(project.id)

    if (projectsStore.getState().page === project.id) {
      setPage('drive')
    }

    await promise
  })
}

/** A function to close all projects. */
export function useCloseAllProjects() {
  const closeProject = useCloseProject()
  const projectsStore = useProjectsStore()
  const removeLaunchedProject = useRemoveLaunchedProject()
  const remoteBackend = backendProvider.useRemoteBackend()
  const localBackend = backendProvider.useLocalBackend()
  const ensureQueryData = useEnsureQueryData()

  return eventCallbacks.useEventCallback(async () => {
    const launchedProjects = projectsStore.getState().launchedProjects

    await Promise.all(
      launchedProjects.map(async (project) => {
        const isHybrid = project.hybrid != null
        const backend =
          project.type === backendModule.BackendType.remote || isHybrid ?
            remoteBackend
          : localBackend
        invariant(backend != null, 'Backend must not be async null')
        const projectDetails = await ensureQueryData(
          createGetProjectDetailsQuery({
            assetId: isHybrid ? project.hybrid.cloudProjectId : project.id,
            backend,
          }),
        )
        if (backendModule.IS_OPENING_OR_OPENED[projectDetails.state.type]) {
          await closeProject(project)
        } else {
          removeLaunchedProject(project.id)
        }
      }),
    )
  })
}
