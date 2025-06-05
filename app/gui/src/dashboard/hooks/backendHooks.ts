/** @file Hooks for interacting with the backend. */
import {
  queryOptions,
  useMutationState,
  useQueryClient,
  type DefaultError,
  type Mutation,
  type MutationKey,
  type QueryClient,
  type QueryKey,
  type UnusedSkipTokenOptions,
  type UseMutationOptions,
  type UseQueryOptions,
} from '@tanstack/react-query'

import {
  backendQueryOptions as backendQueryOptionsBase,
  INVALIDATE_ALL_QUERIES,
  INVALIDATION_MAP,
  type BackendMutationMethod,
  type BackendQueryMethod,
} from 'enso-common/src/backendQuery'

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useOpenProjectLocally, useOpenProjectNatively } from '#/hooks/projectHooks'
import { CATEGORY_TO_FILTER_BY, type Category } from '#/layouts/CategorySwitcher/Category'
import { useSetNewestFolderId, useSetSelectedAssets } from '#/providers/DriveProvider'
import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'
import {
  AssetType,
  BackendType,
  type AnyAsset,
  type AssetId,
  type DirectoryId,
  type FilterBy,
  type User,
  type UserGroupInfo,
} from '#/services/Backend'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { flagsStore } from '$/providers/featureFlags'
import { useFullUserSession } from '$/providers/react'
import { useFeatureFlag } from '$/providers/react/featureFlags'
import { z } from 'zod'

const PROJECT_EXECUTIONS_STALE_TIME = 60_000

export function backendQueryOptions<Method extends BackendQueryMethod>(
  backend: Backend,
  method: Method,
  args: Readonly<Parameters<Backend[Method]>>,
  options?: Omit<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryFn' | 'queryKey'> &
    Partial<Pick<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryKey'>>,
): UnusedSkipTokenOptions<
  Awaited<ReturnType<Backend[Method]>>,
  Error,
  Awaited<ReturnType<Backend[Method]>>,
  QueryKey
>
export function backendQueryOptions<Method extends BackendQueryMethod>(
  backend: Backend | null,
  method: Method,
  args: Readonly<Parameters<Backend[Method]>>,
  options?: Omit<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryFn' | 'queryKey'> &
    Partial<Pick<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryKey'>>,
): UnusedSkipTokenOptions<
  Awaited<ReturnType<Backend[Method]> | undefined>,
  Error,
  Awaited<ReturnType<Backend[Method]> | undefined>,
  QueryKey
>
/** Wrap a backend method call in a React Query. */
export function backendQueryOptions<Method extends BackendQueryMethod>(
  backend: Backend | null,
  method: Method,
  args: Readonly<Parameters<Backend[Method]>>,
  options?: Omit<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryFn' | 'queryKey'> &
    Partial<Pick<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryKey'>>,
) {
  return queryOptions<Awaited<ReturnType<Backend[Method]>>>({
    ...options,
    ...backendQueryOptionsBase(backend, method, args, options?.queryKey),
    queryFn: async () => {
      // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, no-restricted-syntax, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-explicit-any
      let result = await (backend?.[method] as any)?.(...args)
      // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
      switch (method) {
        case 'listUsers': {
          const { multiplyUserList } = flagsStore.getState().featureFlags
          if (multiplyUserList) {
            // eslint-disable-next-line no-restricted-syntax
            const typedResult = result as readonly Omit<User, 'groups'>[]
            const user = typedResult[0]
            result = [
              ...(user != null ?
                [
                  {
                    email: backendModule.EmailAddress('test@example.com'),
                    isEnabled: true,
                    isEnsoTeamMember: false,
                    isOrganizationAdmin: false,
                    name: 'Test User',
                    organizationId: user.organizationId,
                    plan: backendModule.Plan.free,
                    rootDirectoryId: user.rootDirectoryId,
                    userId: user.userId,
                    userGroups: [
                      ...new Set(typedResult.flatMap((otherUser) => otherUser.userGroups ?? [])),
                    ],
                  } satisfies Omit<User, 'groups'>,
                ]
              : []),
              // eslint-disable-next-line @typescript-eslint/no-magic-numbers, @typescript-eslint/no-unsafe-return, @typescript-eslint/no-unsafe-assignment
              ...Array.from({ length: 10 }).flatMap(() => result),
            ]
          }
          break
        }
        default: {
          // No action needed.
          break
        }
      }
      // eslint-disable-next-line @typescript-eslint/no-unsafe-return
      return result
    },
  })
}

/** An identity function to construct options for a mutation. */
export function mutationOptions<
  TData = unknown,
  TError = DefaultError,
  TVariables = void,
  TContext = unknown,
>(
  options: UseMutationOptions<TData, TError, TVariables, TContext>,
): UseMutationOptions<TData, TError, TVariables, TContext> {
  return options
}

/** The type of the corresponding mutation for the given backend method. */
export type BackendMutation<Method extends BackendMutationMethod> = Mutation<
  Awaited<ReturnType<Backend[Method]>>,
  Error,
  Parameters<Backend[Method]>
>

export function backendMutationOptions<Method extends BackendMutationMethod>(
  backend: Backend,
  method: Method,
  options?: Omit<
    UseMutationOptions<Awaited<ReturnType<Backend[Method]>>, Error, Parameters<Backend[Method]>>,
    'mutationFn'
  >,
): UseMutationOptions<Awaited<ReturnType<Backend[Method]>>, Error, Parameters<Backend[Method]>>
export function backendMutationOptions<Method extends BackendMutationMethod>(
  backend: Backend | null,
  method: Method,
  options?: Omit<
    UseMutationOptions<Awaited<ReturnType<Backend[Method]>>, Error, Parameters<Backend[Method]>>,
    'mutationFn'
  >,
): UseMutationOptions<
  Awaited<ReturnType<Backend[Method]>> | undefined,
  Error,
  Parameters<Backend[Method]>
>
/** Wrap a backend method call in a React Query Mutation. */
export function backendMutationOptions<Method extends BackendMutationMethod>(
  backend: Backend | null,
  method: Method,
  options?: Omit<
    UseMutationOptions<Awaited<ReturnType<Backend[Method]>>, Error, Parameters<Backend[Method]>>,
    'mutationFn'
  > & { readonly invalidate?: boolean },
): UseMutationOptions<Awaited<ReturnType<Backend[Method]>>, Error, Parameters<Backend[Method]>> {
  const invalidates =
    options?.invalidate === false ?
      []
    : [
        ...(options?.meta?.invalidates ?? []),
        ...(INVALIDATION_MAP[method]?.map((queryMethod) =>
          queryMethod === INVALIDATE_ALL_QUERIES ? [backend?.type] : [backend?.type, queryMethod],
        ) ?? []),
      ]
  return {
    ...options,
    mutationKey: [backend?.type, method, ...(options?.mutationKey ?? [])],
    // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-return
    mutationFn: (args) => (backend?.[method] as any)?.(...args),
    networkMode: backend?.type === BackendType.local ? 'always' : 'online',
    meta: {
      invalidates,
      awaitInvalidates: options?.meta?.awaitInvalidates ?? true,
      refetchType:
        options?.meta?.refetchType ??
        (invalidates.some((key) => key[1] === 'listDirectory') ? 'all' : 'active'),
    },
  }
}

/** A user group, as well as the users that are a part of the user group. */
export interface UserGroupInfoWithUsers extends UserGroupInfo {
  readonly users: readonly User[]
}

/** Return the refetch interval for listing directories based on feature flag state. */
export function useListDirectoryRefetchInterval() {
  const enableAssetsTableBackgroundRefresh = useFeatureFlag('enableAssetsTableBackgroundRefresh')
  const assetsTableBackgroundRefreshInterval = useFeatureFlag(
    'assetsTableBackgroundRefreshInterval',
  )
  return enableAssetsTableBackgroundRefresh ? assetsTableBackgroundRefreshInterval : Infinity
}

/** Options for {@link listDirectoryQueryOptions}. */
export interface ListDirectoryQueryOptions {
  readonly backend: Backend
  readonly filterBy?: FilterBy | null | undefined
  readonly parentId: DirectoryId | null
  readonly category: Category
  /**
   * When using React, use {@link useListDirectoryRefetchInterval} to get the correct value.
   * `undefined` is intentionally excluded as this value should be explicitly given.
   */
  readonly refetchInterval: number | null
}

/** Build a query options object to fetch the children of a directory. */
export function listDirectoryQueryOptions(options: ListDirectoryQueryOptions) {
  const {
    backend,
    parentId,
    category,
    refetchInterval,
    filterBy = CATEGORY_TO_FILTER_BY[category.type],
  } = options

  const rootPath = 'rootPath' in category ? category.rootPath : undefined

  return queryOptions({
    queryKey: [
      backend.type,
      'listDirectory',
      parentId,
      {
        rootPath,
        labels: null,
        filterBy,
        recentProjects: category.type === 'recent',
      },
    ] as const,
    ...(refetchInterval != null ? { refetchInterval } : {}),
    queryFn: async () => {
      try {
        return await backend.listDirectory(
          {
            parentId,
            rootPath,
            filterBy,
            labels: null,
            recentProjects: category.type === 'recent',
          },
          parentId ?? '(unknown)',
        )
      } catch (error) {
        if (error instanceof Error) {
          throw Object.assign(error, { parentId })
        } else {
          throw error
        }
      }
    },
  })
}

/**
 * Options for {@link unsafe_assetFromCacheQueryOptions}.
 */
export interface AssetFromCacheQueryOptions {
  readonly backend: Backend
  readonly assetId: AssetId
  readonly queryClient: QueryClient
}

/**
 * Build a query options object to fetch an asset from the React Query cache.
 * This is _only_ for situations when WE KNOW that the asset is in the cache.
 * This is _not_ a general purpose function for fetching assets.
 */
// eslint-disable-next-line @typescript-eslint/naming-convention, camelcase
export function unsafe_assetFromCacheQueryOptions(options: AssetFromCacheQueryOptions) {
  const { backend, assetId, queryClient } = options

  const assetSchema = z
    .object({ id: z.string().refine((value) => value === assetId) })
    // This is safe, because we assert that the id is the same as the assetId
    // This makes us sure that this is an asset.
    // eslint-disable-next-line no-restricted-syntax
    .transform((data) => data as unknown as backendModule.AnyAsset)

  return queryOptions({
    queryKey: [backend.type, 'asset', { id: assetId }],
    // We don't want to cache this query, as it's purely a computed from another query.
    gcTime: 0,
    meta: { persist: false },
    queryFn: () =>
      queryClient
        .getQueryCache()
        .getAll()
        .map((query) => {
          const data = query.state.data

          if (Array.isArray(data)) {
            // eslint-disable-next-line no-restricted-syntax
            const asset = data.find((maybeAsset) => assetSchema.safeParse(maybeAsset).success) as
              | AnyAsset
              | undefined

            if (asset != null) {
              return asset
            }
          }

          const result = assetSchema.safeParse(data)

          if (result.success) {
            return result.data
          }

          return null
        })
        .filter((asset) => asset != null)[0],
  })
}

/** The type of directory listings in the React Query cache. */
type DirectoryQuery = readonly AnyAsset<AssetType>[] | undefined

/** Return matching in-flight mutations matching the given filters. */
export function useBackendMutationState<Method extends BackendMutationMethod, Result>(
  backend: Backend,
  method: Method,
  options: {
    mutationKey?: MutationKey
    predicate?: (mutation: BackendMutation<Method>) => boolean
    select?: (mutation: BackendMutation<Method>) => Result
  } = {},
) {
  const { mutationKey, predicate, select } = options
  return useMutationState({
    filters: {
      ...backendMutationOptions(backend, method, mutationKey ? { mutationKey } : {}),
      predicate: (mutation: BackendMutation<Method>) =>
        mutation.state.status === 'pending' && (predicate?.(mutation) ?? true),
    },
    // This is UNSAFE when the `Result` parameter is explicitly specified in the
    // generic parameter list.
    // eslint-disable-next-line no-restricted-syntax
    select: select as (mutation: Mutation<unknown, Error, unknown, unknown>) => Result,
  })
}

/** Return query data for the children of a directory, fetching it if it does not exist. */
export function useEnsureListDirectory(backend: Backend, category: Category) {
  const queryClient = useQueryClient()
  return useEventCallback(async (parentId: DirectoryId) => {
    return await queryClient.ensureQueryData(
      backendQueryOptions(backend, 'listDirectory', [
        {
          parentId,
          labels: null,
          filterBy: CATEGORY_TO_FILTER_BY[category.type],
          recentProjects: category.type === 'recent',
        },
        '(unknown)',
      ]),
    )
  })
}

/**
 * Remove an asset from the React Query cache. Should only be called on
 * optimistically inserted assets.
 */
function useDeleteAsset(backend: Backend, category: Category) {
  const queryClient = useQueryClient()
  const ensureListDirectory = useEnsureListDirectory(backend, category)

  return useEventCallback(async (assetId: AssetId, parentId: DirectoryId) => {
    const siblings = await ensureListDirectory(parentId)
    const asset = siblings.find((sibling) => sibling.id === assetId)
    if (!asset) return

    const listDirectoryQuery = queryClient.getQueryCache().find<DirectoryQuery>({
      queryKey: [
        backend.type,
        'listDirectory',
        parentId,
        {
          labels: null,
          filterBy: CATEGORY_TO_FILTER_BY[category.type],
          recentProjects: category.type === 'recent',
        },
      ],
    })

    if (listDirectoryQuery?.state.data) {
      listDirectoryQuery.setData(
        listDirectoryQuery.state.data.filter((child) => child.id !== assetId),
      )
    }
  })
}

/** A function to create a new folder. */
export function useNewFolder(backend: Backend, category: Category) {
  const ensureListDirectory = useEnsureListDirectory(backend, category)
  const setNewestFolderId = useSetNewestFolderId()
  const setSelectedAssets = useSetSelectedAssets()

  const createDirectoryMutation = useMutationCallback(
    backendMutationOptions(backend, 'createDirectory'),
  )

  return useEventCallback(async (parentId: DirectoryId) => {
    const siblings = await ensureListDirectory(parentId)

    const directoryIndices = siblings
      .filter(backendModule.assetIsDirectory)
      .map((item) => /^New Folder (?<directoryIndex>\d+)$/.exec(item.title))
      .map((match) => match?.groups?.directoryIndex)
      .map((maybeIndex) => (maybeIndex != null ? parseInt(maybeIndex, 10) : 0))

    const title = `New Folder ${Math.max(0, ...directoryIndices) + 1}`
    const placeholderItem = backendModule.createPlaceholderDirectoryAsset(title, parentId)

    return await createDirectoryMutation([
      { parentId: placeholderItem.parentId, title: placeholderItem.title },
    ]).then((result) => {
      setNewestFolderId(result.id)
      setSelectedAssets([{ type: AssetType.directory, ...result }])
      return result
    })
  })
}

/** A function to create a new project. */
export function useNewProject(backend: Backend, category: Category) {
  const ensureListDirectory = useEnsureListDirectory(backend, category)
  const openProjectLocally = useOpenProjectLocally()
  const openProjectNatively = useOpenProjectNatively()
  const deleteAsset = useDeleteAsset(backend, category)

  const createProjectMutation = useMutationCallback(
    backendMutationOptions(backend, 'createProject'),
  )

  return useEventCallback(
    async (
      {
        templateName,
        templateId,
        ensoPath,
      }: {
        templateName: string | null | undefined
        templateId?: string | null | undefined
        ensoPath?: string | null | undefined
      },
      parentId: DirectoryId,
      runLocally = true,
    ) => {
      const siblings = await ensureListDirectory(parentId)
      const projectName = (() => {
        const prefix = `${templateName ?? 'New Project'} `
        const projectNameTemplate = new RegExp(`^${prefix}(?<projectIndex>\\d+)$`)
        const projectIndices = siblings
          .filter(backendModule.assetIsProject)
          .map((item) => projectNameTemplate.exec(item.title)?.groups?.projectIndex)
          .map((maybeIndex) => (maybeIndex != null ? parseInt(maybeIndex, 10) : 0))
        return `${prefix}${Math.max(0, ...projectIndices) + 1}`
      })()

      const placeholderItem = backendModule.createPlaceholderProjectAsset(projectName, parentId)

      return await createProjectMutation([
        {
          parentDirectoryId: placeholderItem.parentId,
          projectName: placeholderItem.title,
          ...(templateId == null ? {} : { projectTemplateName: templateId }),
          ...(ensoPath == null ? {} : { ensoPath }),
        },
      ])
        .catch((error) => {
          void deleteAsset(placeholderItem.id, parentId)
          throw error
        })
        .then((createdProject) => {
          const openProjectParams = {
            id: createdProject.projectId,
            parentId: placeholderItem.parentId,
            title: createdProject.name,
            ...(createdProject.ensoPath != null ? { ensoPath: createdProject.ensoPath } : {}),
          } satisfies Partial<backendModule.ProjectAsset>
          if (runLocally) {
            // Open in background.
            void openProjectLocally(openProjectParams, backend.type)
          } else {
            void openProjectNatively(openProjectParams, backend.type)
          }

          return createdProject
        })
    },
  )
}

/** Remove the user's own permission from an asset. */
export function useRemoveSelfPermissionMutation(backend: Backend) {
  const { user } = useFullUserSession()

  const createPermissionMutation = useMutationCallback(
    backendMutationOptions(backend, 'createPermission', {
      meta: {
        invalidates: [
          [backend.type, 'listDirectory'],
          [backend.type, 'getAssetDetails'],
        ],
        awaitInvalidates: true,
      },
    }),
  )

  const mutate = useEventCallback((id: AssetId) => {
    void createPermissionMutation([
      {
        action: null,
        resourceId: id,
        actorsIds: [user.userId],
      },
    ])
  })

  const mutateAsync = useEventCallback(async (id: AssetId) => {
    await createPermissionMutation([
      {
        action: null,
        resourceId: id,
        actorsIds: [user.userId],
      },
    ])
  })

  return { ...createPermissionMutation, mutate, mutateAsync }
}

/** Build a query options object to list executions for a project. */
export function listProjectExecutionsQueryOptions(
  backend: Backend,
  id: backendModule.ProjectId,
  title: string,
) {
  return queryOptions({
    ...backendQueryOptions(backend, 'listProjectExecutions', [id, title]),
    select: (executions) => [...executions].reverse(),
    staleTime: PROJECT_EXECUTIONS_STALE_TIME,
  })
}

/** Build a query options object to get details for a project execution. */
export function getProjectExecutionDetailsQueryOptions(
  backend: Backend,
  id: backendModule.ProjectExecutionId,
  title: string,
) {
  return queryOptions({
    ...backendQueryOptions(backend, 'getProjectExecutionDetails', [id, title]),
    staleTime: PROJECT_EXECUTIONS_STALE_TIME,
  })
}
