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

import { useFeatureFlag } from '#/providers/FeatureFlagsProvider'
import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'

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
  const enableCloudExecution = useFeatureFlag('enableCloudExecution')
  return enableCloudExecution
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
export const STATIC_PROJECT_STATES = new Set([
  backendModule.ProjectState.opened,
  backendModule.ProjectState.closed,
])
export const CREATED_PROJECT_STATES = new Set([
  backendModule.ProjectState.created,
  backendModule.ProjectState.new,
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

      return DEFAULT_INTERVAL_MS
    },
  })
}
createGetProjectDetailsQuery.getQueryKey = (id: LaunchedProjectId) => ['project', id] as const

/** A mutation to open a project. */
export function useOpenProjectMutation() {
  const client = reactQuery.useQueryClient()
  const session = authProvider.useFullUserSession()
  const remoteBackend = backendProvider.useRemoteBackend()
  const localBackend = backendProvider.useLocalBackend()
  const setProjectAsset = useSetProjectAsset()

  return reactQuery.useMutation({
    mutationKey: ['openProject'],
    networkMode: 'always',
    mutationFn: ({
      title,
      id,
      type,
      parentId,
      inBackground = false,
    }: LaunchedProject & { inBackground?: boolean }) => {
      const backend = type === backendModule.BackendType.remote ? remoteBackend : localBackend

      invariant(backend != null, 'Backend is null')

      return backend.openProject(
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
          parentId,
        },
        title,
      )
    },
    onMutate: ({ type, id, parentId }) => {
      const queryKey = createGetProjectDetailsQuery.getQueryKey(id)

      client.setQueryData(queryKey, { state: { type: backendModule.ProjectState.openInProgress } })
      setProjectAsset(type, id, parentId, (asset) => ({
        ...asset,
        projectState: { ...asset.projectState, type: backendModule.ProjectState.openInProgress },
      }))

      void client.cancelQueries({ queryKey })
    },
    onSuccess: async (_, { type, id, parentId }) => {
      await client.resetQueries({ queryKey: createGetProjectDetailsQuery.getQueryKey(id) })
      await client.invalidateQueries({ queryKey: [type, 'listDirectory', parentId] })
    },
    onError: async (_, { type, id, parentId }) => {
      await client.invalidateQueries({ queryKey: createGetProjectDetailsQuery.getQueryKey(id) })
      await client.invalidateQueries({ queryKey: [type, 'listDirectory', parentId] })
    },
  })
}

/** Mutation to close a project. */
export function useCloseProjectMutation() {
  const client = reactQuery.useQueryClient()
  const remoteBackend = backendProvider.useRemoteBackend()
  const localBackend = backendProvider.useLocalBackend()
  const setProjectAsset = useSetProjectAsset()

  return reactQuery.useMutation({
    mutationKey: ['closeProject'],
    mutationFn: ({ type, id, title }: LaunchedProject) => {
      const backend = type === backendModule.BackendType.remote ? remoteBackend : localBackend

      invariant(backend != null, 'Backend is null')

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
    onSuccess: async (_, { type, id, parentId }) => {
      await client.resetQueries({ queryKey: createGetProjectDetailsQuery.getQueryKey(id) })
      setProjectAsset(type, id, parentId, (asset) => ({
        ...asset,
        projectState: { ...asset.projectState, type: backendModule.ProjectState.closed },
      }))
    },
    onError: async (_, { type, id, parentId }) => {
      await client.invalidateQueries({ queryKey: createGetProjectDetailsQuery.getQueryKey(id) })
      await client.invalidateQueries({ queryKey: [type, 'listDirectory', parentId] })
    },
  })
}

/** Mutation to rename a project. */
export function useRenameProjectMutation() {
  const client = reactQuery.useQueryClient()
  const remoteBackend = backendProvider.useRemoteBackend()
  const localBackend = backendProvider.useLocalBackend()
  const updateLaunchedProjects = useUpdateLaunchedProjects()

  return reactQuery.useMutation({
    mutationKey: ['renameProject'],
    mutationFn: ({ newName, project }: { newName: string; project: LaunchedProject }) => {
      const { type, id, title } = project
      const backend = type === backendModule.BackendType.remote ? remoteBackend : localBackend

      invariant(backend != null, 'Backend is null')

      return backend.updateProject(id, { projectName: newName, ami: null, ideVersion: null }, title)
    },
    onSuccess: (_, { newName, project }) => {
      updateLaunchedProjects((projects) =>
        projects.map((otherProject) =>
          project.id !== otherProject.id ? otherProject : merge(otherProject, { title: newName }),
        ),
      )
      return client.invalidateQueries({
        queryKey: createGetProjectDetailsQuery.getQueryKey(project.id),
      })
    },
  })
}

/** A callback to open a project. */
export function useOpenProject() {
  const client = reactQuery.useQueryClient()
  const canOpenProjects = useCanOpenProjects()
  const projectsStore = useProjectsStore()
  const addLaunchedProject = useAddLaunchedProject()
  const closeAllProjects = useCloseAllProjects()
  const openProjectMutation = useOpenProjectMutation()

  const enableMultitabs = useFeatureFlag('enableMultitabs')

  return eventCallbacks.useEventCallback((project: LaunchedProject) => {
    if (!canOpenProjects) {
      return
    }

    if (!enableMultitabs) {
      // Since multiple tabs cannot be opened at the same time, the opened projects need to be closed first.
      if (projectsStore.getState().launchedProjects.length > 0) {
        closeAllProjects()
      }
    }

    const existingMutation = client.getMutationCache().find({
      mutationKey: ['openProject'],
      predicate: (mutation) => mutation.options.scope?.id === project.id,
    })
    const isOpeningTheSameProject = existingMutation?.state.status === 'pending'

    if (!isOpeningTheSameProject) {
      openProjectMutation.mutate(project)
      const openingProjectMutation = client.getMutationCache().find({
        mutationKey: ['openProject'],
        // this is unsafe, but we can't do anything about it
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
        predicate: (mutation) => mutation.state.variables?.id === project.id,
      })
      openingProjectMutation?.setOptions({
        ...openingProjectMutation.options,
        scope: { id: project.id },
      })

      addLaunchedProject(project)
    }
  })
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

  return eventCallbacks.useEventCallback((project: LaunchedProject) => {
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

    closeProjectMutation.mutate(project)

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
  })
}

/** A function to close all projects. */
export function useCloseAllProjects() {
  const closeProject = useCloseProject()
  const projectsStore = useProjectsStore()

  return eventCallbacks.useEventCallback(() => {
    const launchedProjects = projectsStore.getState().launchedProjects

    for (const launchedProject of launchedProjects) {
      closeProject(launchedProject)
    }
  })
}
