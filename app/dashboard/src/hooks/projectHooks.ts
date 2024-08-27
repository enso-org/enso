/** @file Mutations related to project management. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'
import invariant from 'tiny-invariant'

import { merge } from 'enso-common/src/utilities/data/object'

import * as eventCallbacks from '#/hooks/eventCallbackHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import {
  TabType,
  useAddLaunchedProject,
  useProjectsStore,
  useRemoveLaunchedProject,
  useSetPage,
  useUpdateLaunchedProjects,
  type LaunchedProject,
  type LaunchedProjectId,
} from '#/providers/ProjectsProvider'

import * as backendModule from '#/services/Backend'
import type LocalBackend from '#/services/LocalBackend'
import type RemoteBackend from '#/services/RemoteBackend'

// ====================================
// === createGetProjectDetailsQuery ===
// ====================================
/** Default interval for refetching project status when the project is opened. */
const OPENED_INTERVAL_MS = 30_000
/** Interval when we open a cloud project.
 * Since opening a cloud project is a long operation, we want to check the status less often. */
const CLOUD_OPENING_INTERVAL_MS = 5_000
/** Interval when we open a local project or when we want to sync the project status as soon as
 * possible. */
const ACTIVE_SYNC_INTERVAL_MS = 100

/** Options for {@link createGetProjectDetailsQuery}. */
export interface CreateOpenedProjectQueryOptions {
  readonly type: backendModule.BackendType
  readonly assetId: backendModule.Asset<backendModule.AssetType.project>['id']
  readonly parentId: backendModule.Asset<backendModule.AssetType.project>['parentId']
  readonly title: backendModule.Asset<backendModule.AssetType.project>['title']
  readonly remoteBackend: RemoteBackend
  readonly localBackend: LocalBackend | null
}

/** Project status query.  */
export function createGetProjectDetailsQuery(options: CreateOpenedProjectQueryOptions) {
  const { assetId, parentId, title, remoteBackend, localBackend, type } = options

  const backend = type === backendModule.BackendType.remote ? remoteBackend : localBackend
  const isLocal = type === backendModule.BackendType.local

  return reactQuery.queryOptions({
    queryKey: createGetProjectDetailsQuery.getQueryKey(assetId),
    meta: { persist: false },
    gcTime: 0,
    refetchInterval: ({ state }) => {
      const states = [backendModule.ProjectState.opened, backendModule.ProjectState.closed]

      if (state.status === 'error') {
        // eslint-disable-next-line no-restricted-syntax
        return false
      }
      if (isLocal) {
        if (state.data?.state.type === backendModule.ProjectState.opened) {
          return OPENED_INTERVAL_MS
        } else {
          return ACTIVE_SYNC_INTERVAL_MS
        }
      } else {
        if (state.data == null) {
          return ACTIVE_SYNC_INTERVAL_MS
        } else if (states.includes(state.data.state.type)) {
          return OPENED_INTERVAL_MS
        } else {
          return CLOUD_OPENING_INTERVAL_MS
        }
      }
    },
    refetchIntervalInBackground: true,
    refetchOnWindowFocus: true,
    refetchOnMount: true,
    queryFn: async () => {
      invariant(backend != null, 'Backend is null')

      return await backend.getProjectDetails(assetId, parentId, title)
    },
  })
}
createGetProjectDetailsQuery.getQueryKey = (id: LaunchedProjectId) => ['project', id] as const
createGetProjectDetailsQuery.createPassiveListener = (id: LaunchedProjectId) =>
  reactQuery.queryOptions<backendModule.Project | null>({
    queryKey: createGetProjectDetailsQuery.getQueryKey(id),
    initialData: null,
  })

// ==============================
// === useOpenProjectMutation ===
// ==============================

/** A mutation to open a project. */
export function useOpenProjectMutation() {
  const client = reactQuery.useQueryClient()
  const session = authProvider.useFullUserSession()
  const remoteBackend = backendProvider.useRemoteBackendStrict()
  const localBackend = backendProvider.useLocalBackend()

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
            refreshToken: session.accessToken,
            clientId: session.clientId,
            expireAt: session.expireAt,
            refreshUrl: session.refreshUrl,
          },
          parentId,
        },
        title,
      )
    },
    onMutate: ({ id }) => {
      const queryKey = createGetProjectDetailsQuery.getQueryKey(id)

      client.setQueryData(queryKey, { state: { type: backendModule.ProjectState.openInProgress } })

      void client.cancelQueries({ queryKey })
    },
    onSuccess: (_, { id }) =>
      client.resetQueries({ queryKey: createGetProjectDetailsQuery.getQueryKey(id) }),
    onError: (_, { id }) =>
      client.invalidateQueries({ queryKey: createGetProjectDetailsQuery.getQueryKey(id) }),
  })
}

// ===============================
// === useCloseProjectMutation ===
// ===============================

/** Mutation to close a project. */
export function useCloseProjectMutation() {
  const client = reactQuery.useQueryClient()
  const remoteBackend = backendProvider.useRemoteBackendStrict()
  const localBackend = backendProvider.useLocalBackend()

  return reactQuery.useMutation({
    mutationKey: ['closeProject'],
    mutationFn: ({ type, id, title }: LaunchedProject) => {
      const backend = type === backendModule.BackendType.remote ? remoteBackend : localBackend

      invariant(backend != null, 'Backend is null')

      return backend.closeProject(id, title)
    },
    onMutate: ({ id }) => {
      const queryKey = createGetProjectDetailsQuery.getQueryKey(id)

      client.setQueryData(queryKey, { state: { type: backendModule.ProjectState.closing } })

      void client.cancelQueries({ queryKey })
    },
    onSuccess: (_, { id }) =>
      client.resetQueries({ queryKey: createGetProjectDetailsQuery.getQueryKey(id) }),
    onError: (_, { id }) =>
      client.invalidateQueries({ queryKey: createGetProjectDetailsQuery.getQueryKey(id) }),
  })
}

// ================================
// === useRenameProjectMutation ===
// ================================

/** Mutation to rename a project. */
export function useRenameProjectMutation() {
  const client = reactQuery.useQueryClient()
  const remoteBackend = backendProvider.useRemoteBackendStrict()
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

// ======================
// === useOpenProject ===
// ======================

/** A callback to open a project. */
export function useOpenProject() {
  const client = reactQuery.useQueryClient()
  const projectsStore = useProjectsStore()
  const addLaunchedProject = useAddLaunchedProject()
  const closeAllProjects = useCloseAllProjects()
  const openProjectMutation = useOpenProjectMutation()
  return eventCallbacks.useEventCallback((project: LaunchedProject) => {
    // Since multiple tabs cannot be opened at the sametime, the opened projects need to be closed first.
    if (projectsStore.getState().launchedProjects.length > 0) {
      closeAllProjects()
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

// =====================
// === useOpenEditor ===
// =====================

/** A function to open the editor. */
export function useOpenEditor() {
  const setPage = useSetPage()
  return eventCallbacks.useEventCallback((projectId: LaunchedProjectId) => {
    React.startTransition(() => {
      setPage(projectId)
    })
  })
}

// =======================
// === useCloseProject ===
// =======================

/** A function to close a project. */
export function useCloseProject() {
  const client = reactQuery.useQueryClient()
  const closeProjectMutation = useCloseProjectMutation()
  const removeLaunchedProject = useRemoveLaunchedProject()
  const projectsStore = useProjectsStore()
  const setPage = useSetPage()

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
        // this is unsafe, but we can't do anything about it
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
        predicate: (mutation) => mutation.state.variables?.id === project.id,
      })
      .forEach((mutation) => {
        mutation.setOptions({ ...mutation.options, scope: { id: project.id } })
      })
    removeLaunchedProject(project.id)

    if (projectsStore.getState().page === project.id) {
      setPage(TabType.drive)
    }
  })
}

// ===========================
// === useCloseAllProjects ===
// ===========================

/** A function to close all projects. */
export function useCloseAllProjects() {
  const projectsStore = useProjectsStore()
  const closeProject = useCloseProject()
  return eventCallbacks.useEventCallback(() => {
    for (const launchedProject of projectsStore.getState().launchedProjects) {
      closeProject(launchedProject)
    }
  })
}
