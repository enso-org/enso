/** @file Mutations related to project management. */
import { queryOptions, useMutation, useQueryClient, type InfiniteData } from '@tanstack/react-query'
import invariant from 'tiny-invariant'

import { merge } from 'enso-common/src/utilities/data/object'

import * as eventCallbacks from '#/hooks/eventCallbackHooks'

import type { LaunchedProject, LaunchedProjectId } from '$/providers/container'
import * as authProvider from '$/providers/react'
import {
  useAddClosingProject,
  useAddLaunchedProject,
  useAddOpeningProject,
  useContainerData,
  useRemoveClosingProject,
  useRemoveLaunchedProject,
  useRemoveOpeningProject,
  useUpdateLaunchedProjects,
} from '$/providers/react/container'

import { useCanRunProjects } from '#/hooks/backendHooks'
import { useUploadFile } from '#/hooks/backendUploadFilesHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import { useLogger } from '#/providers/LoggerProvider'
import type Backend from '#/services/Backend'
import {
  AssetType,
  BackendType,
  EnsoPath,
  extractTypeAndPath,
  IS_OPENING_OR_OPENED,
  ProjectState,
  type Asset,
  type AssetId,
  type DirectoryId,
  type ListDirectoryResponseBody,
  type Project,
  type ProjectAsset,
} from '#/services/Backend'
import { assert } from '#/utilities/error'
import { usePreventNavigation } from '#/utilities/preventNavigation'
import { useBackends, useText } from '$/providers/react'
import { useFeatureFlag } from '$/providers/react/featureFlags'
import { useState } from 'react'
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
const OPEN_IN_PROGRESS_PROJECT_STATE_SCHEMA = z.object({
  state: z.object({ type: z.literal(ProjectState.openInProgress) }),
})

/** Options for {@link createGetProjectDetailsQuery}. */
export interface CreateOpenedProjectQueryOptions {
  readonly assetId: Asset<AssetType.project>['id']
  readonly backend: Backend
}

/** Return a function to update a project asset in the TanStack Query cache. */
function useSetProjectAsset() {
  const queryClient = useQueryClient()
  return eventCallbacks.useEventCallback(
    (
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
    },
  )
}

export const OPENING_PROJECT_STATES = new Set([
  ProjectState.provisioned,
  ProjectState.scheduled,
  ProjectState.openInProgress,
])
export const OPENED_PROJECT_STATES = new Set([ProjectState.opened])
export const CLOSED_PROJECT_STATES = new Set([ProjectState.closed])
export const STATIC_PROJECT_STATES = new Set([ProjectState.opened, ProjectState.closed])
export const CREATED_PROJECT_STATES = new Set([ProjectState.created, ProjectState.new])
export const BUSY_PROJECT_STATES = new Set([
  ...Array.from(OPENING_PROJECT_STATES),
  ProjectState.opened,
  ProjectState.hybridOpened,
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
export function getTimeoutBasedOnTheBackendType(backendType: BackendType) {
  switch (backendType) {
    case BackendType.local: {
      return LOCAL_PROJECT_OPEN_TIMEOUT_MS
    }
    case BackendType.remote: {
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

  const isLocal = backend.type === BackendType.local

  return queryOptions({
    queryKey: getProjectDetailsQueryKey(assetId),
    queryFn: () => backend.getProjectDetails(assetId),
    refetchIntervalInBackground: true,
    refetchOnMount: true,
    networkMode: backend.type === BackendType.remote ? 'online' : 'always',
    meta: { persist: false },
    refetchInterval: ({ state }): number | false => {
      if (state.status === 'error' || !state.data) {
        return false
      }
      if (CREATED_PROJECT_STATES.has(state.data.state.type)) {
        return isLocal ? LOCAL_OPENING_INTERVAL_MS : CLOUD_OPENING_INTERVAL_MS
      }
      if (STATIC_PROJECT_STATES.has(state.data.state.type)) {
        return OPENED_INTERVAL_MS
      }
      if (OPENING_PROJECT_STATES.has(state.data.state.type)) {
        return isLocal ? LOCAL_OPENING_INTERVAL_MS : CLOUD_OPENING_INTERVAL_MS
      }
      return DEFAULT_INTERVAL_MS
    },
  })
}

/** Create a query key for `getProjectDetails`. */
export function getProjectDetailsQueryKey(id: LaunchedProjectId) {
  return ['project', id] as const
}

const OPEN_PROJECT_MUTATION_KEY = ['openProject'] as const

/** A mutation to open a project in backend. */
export function useOpenProjectMutation() {
  const client = useQueryClient()
  const session = authProvider.useFullUserSession()
  const { remoteBackend, localBackend } = useBackends()
  const setProjectAsset = useSetProjectAsset()
  const addOpeningProject = useAddOpeningProject()
  const removeOpeningProject = useRemoveOpeningProject()
  const { closingProjects } = useContainerData()

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
      assert(() => !closingProjects.has(id))
      addOpeningProject(hybrid?.cloudProjectId ?? id, ensoPath)
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
          removeOpeningProject(hybrid?.cloudProjectId ?? id)
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
    onSuccess: async (_data, { title, hybrid, suppressHybridProjectOpen = false }) => {
      if (hybrid && !suppressHybridProjectOpen) {
        await remoteBackend.setHybridOpened(hybrid.cloudProjectId, title)
      }
    },
    onError: async (_error, { type, parentId }) => {
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
  const logger = useLogger()
  const { getText } = useText()
  const client = useQueryClient()
  const { remoteBackend, localBackend } = useBackends()
  const uploadFile = useUploadFile(remoteBackend, { updateProgress: false })
  const [isHybridPending, setIsHybridPending] = useState(false)
  const toastAndLog = useToastAndLog()
  const addClosingProject = useAddClosingProject()
  const removeClosingProject = useRemoveClosingProject()
  usePreventNavigation({ message: getText('anUploadIsInProgress'), isEnabled: isHybridPending })

  return useMutationCallback({
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
        setIsHybridPending(true)
        addClosingProject(hybrid.cloudProjectId)
      } else {
        addClosingProject(id)
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
        ]).catch((error) => {
          toastAndLog('uploadProjectError', error)
        })
        invariant(localBackend != null, 'LocalBackend is null')
        await localBackend
          .deleteAsset(hybrid.parentId, { force: true }, null)
          .catch((error) => logger.error('Failed to remove local version of hybrid project', error))
        setIsHybridPending(false)
        removeClosingProject(hybrid.cloudProjectId)
      } else {
        removeClosingProject(id)
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
        ]).catch((error) => {
          toastAndLog('uploadProjectError', error)
        })

        invariant(localBackend != null, 'LocalBackend is null')
        await localBackend
          .deleteAsset(hybrid.parentId, { force: true }, null)
          .catch((error) => logger.error('Failed to remove local version of hybrid project', error))
        removeClosingProject(hybrid.cloudProjectId)
        setIsHybridPending(false)
        await client.invalidateQueries({
          queryKey: getProjectDetailsQueryKey(hybrid.cloudProjectId),
        })
        await client.invalidateQueries({
          queryKey: [BackendType.remote, 'listDirectory', hybrid.cloudParentId],
        })
      } else {
        removeClosingProject(id)
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
  const updateLaunchedProjects = useUpdateLaunchedProjects()
  const client = useQueryClient()

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

/** A callback to open a project. */
function useOpenProject() {
  const client = useQueryClient()
  const containerData = useContainerData()
  const addOpeningProject = useAddOpeningProject()
  const removeOpeningProject = useRemoveOpeningProject()
  const addLaunchedProject = useAddLaunchedProject()
  const removeLaunchedProject = useRemoveLaunchedProject()
  const closeAllProjects = useCloseAllProjects()
  const openProjectMutation = useOpenProjectMutation()
  const enableMultitabs = useFeatureFlag('enableMultitabs')

  return eventCallbacks.useEventCallback(async (project: LaunchedProject) => {
    const existingMutation = client.getMutationCache().find({
      mutationKey: ['openProject'],
      predicate: (mutation) => mutation.options.scope?.id === project.id,
    })
    const isOpeningTheSameProject = existingMutation?.state.status === 'pending'
    if (!isOpeningTheSameProject) {
      const queryKey = getProjectDetailsQueryKey(project.id)
      client.setQueryData(queryKey, { state: { type: ProjectState.openInProgress } })

      addOpeningProject(project.hybrid?.cloudProjectId ?? project.id, project.ensoPath)

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
function useOpenHybridProject() {
  const { localBackend, remoteBackend } = useBackends()
  const toastAndLog = useToastAndLog()
  const openProject = useOpenProject()
  const closeProject = useCloseProject()
  const addOpeningProject = useAddOpeningProject()
  const removeOpeningProject = useRemoveOpeningProject()

  return eventCallbacks.useEventCallback(
    async (asset: Pick<ProjectAsset, 'ensoPath' | 'id' | 'parentId' | 'title'>) => {
      let launchedProject: LaunchedProject | undefined

      try {
        invariant(localBackend != null, 'Local Backend is null')
        addOpeningProject(asset.id, asset.ensoPath)
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
        toastAndLog('openProjectError', error, asset.title)
        await Promise.allSettled([
          closeProject({ ...asset, type: BackendType.remote }),
          ...(launchedProject ? [closeProject(launchedProject)] : []),
        ])
      } finally {
        removeOpeningProject(asset.id)
      }
    },
  )
}

/** Return a function to reopen a previously opened project that has since been closed. */
export function useReopenProject(openProjectMutation: ReturnType<typeof useOpenProjectMutation>) {
  const { remoteBackend } = useBackends()

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
  const canRunProjects = useCanRunProjects()
  const openProject = useOpenProject()

  return eventCallbacks.useEventCallback(
    async (
      asset: Pick<ProjectAsset, 'ensoPath' | 'id' | 'parentId' | 'title'>,
      backendType: BackendType,
    ) => {
      if (!canRunProjects.natively[backendType]) {
        return
      }
      await openProject({ ...asset, type: backendType })
    },
  )
}

/** Return a function to open a project locally - meaning Hybrid Mode is used for Cloud projects. */
export function useOpenProjectLocally() {
  const openProject = useOpenProject()
  const canRunProjects = useCanRunProjects()
  const openHybridProject = useOpenHybridProject()

  return eventCallbacks.useEventCallback(
    async (
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
    },
  )
}

/** A function to close a project. */
export function useCloseProject() {
  const client = useQueryClient()
  const closeProjectMutation = useCloseProjectMutation()
  const removeLaunchedProject = useRemoveLaunchedProject()

  return eventCallbacks.useEventCallback(
    async (project: Pick<LaunchedProject, 'hybrid' | 'id' | 'parentId' | 'title' | 'type'>) => {
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

      await promise
    },
  )
}

/** A function to close all projects. */
export function useCloseAllProjects() {
  const closeProject = useCloseProject()
  const containerData = useContainerData()
  const removeLaunchedProject = useRemoveLaunchedProject()
  const removeOpeningProject = useRemoveOpeningProject()
  const { remoteBackend, localBackend } = useBackends()
  const ensureQueryData = useEnsureQueryData()

  return eventCallbacks.useEventCallback(async () => {
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
            removeLaunchedProject(project.id)
          }
        } else {
          removeOpeningProject(project.id)
        }
      }),
    )
  })
}
