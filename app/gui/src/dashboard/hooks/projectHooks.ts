/** @file Mutations related to project management. */
import * as reactQuery from '@tanstack/react-query'
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
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import { useLogger } from '#/providers/LoggerProvider'
import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'
import { assert } from '#/utilities/error'
import { usePreventNavigation } from '#/utilities/preventNavigation'
import { useBackends, useText } from '$/providers/react'
import { useFeatureFlag } from '$/providers/react/featureFlags'
import { useUploadsToCloudStore } from '$/providers/react/upload'
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

/** Options for {@link createGetProjectDetailsQuery}. */
export interface CreateOpenedProjectQueryOptions {
  readonly assetId: backendModule.Asset<backendModule.AssetType.project>['id']
  readonly backend: Backend
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
        .find<backendModule.ListDirectoryResponseBody | undefined>({
          queryKey: [backendType, 'listDirectory', parentId, { infinite: false }],
          exact: false,
        })

      if (listDirectoryQuery?.state.data) {
        listDirectoryQuery.setData({
          ...listDirectoryQuery.state.data,
          assets: listDirectoryQuery.state.data.assets.map((child) =>
            child.id === assetId && child.type === backendModule.AssetType.project ?
              transform(child)
            : child,
          ),
        })
      }

      const listDirectoryInfiniteQuery = queryClient
        .getQueryCache()
        .find<reactQuery.InfiniteData<backendModule.ListDirectoryResponseBody> | undefined>({
          queryKey: [backendType, 'listDirectory', parentId, { infinite: true }],
          exact: false,
        })

      if (listDirectoryInfiniteQuery?.state.data) {
        listDirectoryInfiniteQuery.setData({
          ...listDirectoryInfiniteQuery.state.data,
          pages: listDirectoryInfiniteQuery.state.data.pages.map((page) => ({
            ...page,
            assets: page.assets.map((child) =>
              child.id === assetId && child.type === backendModule.AssetType.project ?
                transform(child)
              : child,
            ),
          })),
        })
      }
    },
  )
}

export const OPENING_PROJECT_STATES = new Set([
  backendModule.ProjectState.provisioned,
  backendModule.ProjectState.scheduled,
  backendModule.ProjectState.openInProgress,
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
export const BUSY_PROJECT_STATES = new Set([
  ...Array.from(OPENING_PROJECT_STATES),
  backendModule.ProjectState.opened,
  backendModule.ProjectState.hybridOpened,
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

const OPEN_PROJECT_MUTATION_KEY = ['openProject'] as const

/** A mutation to open a project in backend. */
export function useOpenProjectMutation() {
  const client = reactQuery.useQueryClient()
  const session = authProvider.useFullUserSession()
  const { remoteBackend, localBackend } = useBackends()
  const setProjectAsset = useSetProjectAsset()
  const addOpeningProject = useAddOpeningProject()
  const removeOpeningProject = useRemoveOpeningProject()
  const { closingProjects } = useContainerData()

  return reactQuery.useMutation({
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
      const backend = type === backendModule.BackendType.remote ? remoteBackend : localBackend

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

      client.setQueryData(queryKey, { state: { type: backendModule.ProjectState.openInProgress } })
      setProjectAsset(type, id, parentId, (asset) => ({
        ...asset,
        projectState: { ...asset.projectState, type: backendModule.ProjectState.openInProgress },
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
  const { getText } = useText()
  const client = reactQuery.useQueryClient()
  const logger = useLogger()
  const { remoteBackend, localBackend } = useBackends()
  const [isHybridPending, setIsHybridPending] = useState(false)
  const uploads = useUploadsToCloudStore()
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
      const backend = type === backendModule.BackendType.remote ? remoteBackend : localBackend

      invariant(backend != null, 'Backend is null')

      if (hybrid) {
        await remoteBackend.closeProject(hybrid.cloudProjectId, title)
      }

      return backend.closeProject(id, title)
    },
    onMutate: ({ hybrid, id }) => {
      const queryKey = createGetProjectDetailsQuery.getQueryKey(id)

      if (hybrid) {
        setIsHybridPending(true)
        addClosingProject(hybrid.cloudProjectId)
      } else {
        addClosingProject(id)
      }

      void client.cancelQueries({ queryKey })
    },
    onSuccess: async (_, { type, id, parentId, hybrid }) => {
      await client.resetQueries({ queryKey: createGetProjectDetailsQuery.getQueryKey(id) })

      if (hybrid) {
        const fileName = 'project_root.enso-project'
        const file = await remoteBackend.getProjectArchive(parentId, fileName)
        await uploads
          .uploadFile(
            file,
            {
              fileId: hybrid.cloudProjectId,
              fileName,
              parentDirectoryId: hybrid.cloudParentId,
            },
            'hybridSync',
          )
          .catch((error) => {
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

      await client.invalidateQueries({ queryKey: createGetProjectDetailsQuery.getQueryKey(id) })
      await client.invalidateQueries({ queryKey: [type, 'listDirectory', parentId] })
    },
    onError: async (_, { type, id, parentId, hybrid }) => {
      if (hybrid) {
        const fileName = 'project_root.enso-project'
        const file = await remoteBackend.getProjectArchive(parentId, fileName)
        await uploads
          .uploadFile(
            file,
            {
              fileId: hybrid.cloudProjectId,
              fileName,
              parentDirectoryId: hybrid.cloudParentId,
            },
            'hybridSync',
          )
          .catch((error) => {
            toastAndLog('uploadProjectError', error)
          })

        invariant(localBackend != null, 'LocalBackend is null')
        await localBackend
          .deleteAsset(hybrid.parentId, { force: true }, null)
          .catch((error) => logger.error('Failed to remove local version of hybrid project', error))
        removeClosingProject(hybrid.cloudProjectId)
        setIsHybridPending(false)
        await client.invalidateQueries({
          queryKey: createGetProjectDetailsQuery.getQueryKey(hybrid.cloudProjectId),
        })
        await client.invalidateQueries({
          queryKey: [backendModule.BackendType.remote, 'listDirectory', hybrid.cloudParentId],
        })
      } else {
        removeClosingProject(id)
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
  const client = reactQuery.useQueryClient()

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
      const queryKey = createGetProjectDetailsQuery.getQueryKey(project.id)
      await client.cancelQueries({
        queryKey,
      })
      // Optimistically update the project name.
      client.setQueryData<backendModule.Project>(queryKey, (data) => {
        if (data == null) return undefined
        return {
          ...data,
          name: newName,
        }
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

const OPEN_IN_PROGRESS_PROJECT_STATE_SCHEMA = z.object({
  state: z.object({ type: z.literal(backendModule.ProjectState.openInProgress) }),
})

/** A callback to open a project. */
function useOpenProject() {
  const client = reactQuery.useQueryClient()
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
      const queryKey = createGetProjectDetailsQuery.getQueryKey(project.id)
      client.setQueryData(queryKey, { state: { type: backendModule.ProjectState.openInProgress } })

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
function useOpenHybridProject() {
  const { localBackend, remoteBackend } = useBackends()
  const toastAndLog = useToastAndLog()
  const openProject = useOpenProject()
  const closeProject = useCloseProject()
  const addOpeningProject = useAddOpeningProject()
  const removeOpeningProject = useRemoveOpeningProject()

  return eventCallbacks.useEventCallback(
    async (asset: Pick<backendModule.ProjectAsset, 'ensoPath' | 'id' | 'parentId' | 'title'>) => {
      let launchedProject: LaunchedProject | undefined

      try {
        invariant(localBackend != null, 'Local Backend is null')
        addOpeningProject(asset.id, asset.ensoPath)
        const projectSessionId = await remoteBackend.setHybridOpenInProgress(asset.id, asset.title)
        const localProject = await remoteBackend.downloadProject(asset.id)
        const cloudProjectDirectoryPath = backendModule.EnsoPath(
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
          project = assets.filter((item) => item.type === backendModule.AssetType.project).at(0)
          if (project) {
            break
          }
        }

        invariant(project, 'Downloaded cloud project does not exist in Local Backend.')
        launchedProject = {
          id: project.id,
          title: asset.title,
          parentId: project.parentId,
          ensoPath: asset.ensoPath,
          type: backendModule.BackendType.local,
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
          closeProject({ ...asset, type: backendModule.BackendType.remote }),
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
      asset: Pick<backendModule.ProjectAsset, 'ensoPath' | 'id' | 'parentId' | 'title'>,
      backendType: backendModule.BackendType,
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
      asset: Pick<backendModule.ProjectAsset, 'ensoPath' | 'id' | 'parentId' | 'title'>,
      backendType: backendModule.BackendType,
    ) => {
      if (!canRunProjects.locally[backendType]) {
        return
      }
      const isCloud = backendType === backendModule.BackendType.remote
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
  const client = reactQuery.useQueryClient()
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
    const launchedProjects = containerData.openedProjects

    await Promise.all(
      launchedProjects.map(async (project) => {
        if (project.state === 'launched') {
          const backend =
            project.type === backendModule.BackendType.remote || project.hybrid != null ?
              remoteBackend
            : localBackend
          invariant(backend != null, 'Backend must not be async null')
          const projectDetails = await ensureQueryData(
            createGetProjectDetailsQuery({
              assetId: project.hybrid != null ? project.hybrid.cloudProjectId : project.id,
              backend,
            }),
          )
          if (backendModule.IS_OPENING_OR_OPENED[projectDetails.state.type]) {
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
