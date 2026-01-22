/** @file Hooks for interacting with the backend. */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { CATEGORY_TO_FILTER_BY, type Category } from '#/layouts/CategorySwitcher/Category'
import { useSetAssetToRename, useSetSelectedAssets } from '#/providers/DriveProvider'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import type { ProjectInfo } from '$/providers/openedProjects/projectInfo'
import { useFullUserSession } from '$/providers/react'
import { useFeatureFlag } from '$/providers/react/featureFlags'
import { useOpenedProjects } from '$/providers/react/openedProjects'
import {
  backendQueryOptions as backendQueryOptionsBase,
  INVALIDATE_ALL_QUERIES,
  INVALIDATION_MAP,
  PERSISTENCE_MAP,
  STALE_TIME_MAP,
  type BackendMutationMethod,
  type BackendQueryMethod,
} from '$/utils/backendQuery'
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
import type { Backend } from 'enso-common/src/services/Backend'
import * as backendModule from 'enso-common/src/services/Backend'
import {
  AssetType,
  BackendType,
  type AnyAsset,
  type AssetId,
  type DirectoryId,
  type FilterBy,
  type User,
  type UserGroupInfo,
} from 'enso-common/src/services/Backend'
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
    staleTime: options?.staleTime ?? STALE_TIME_MAP[method] ?? 0,
    meta: { ...options?.meta, persist: PERSISTENCE_MAP[method] ?? options?.meta?.persist ?? true },
    // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-return
    queryFn: () => (backend?.[method] as any)?.(...args),
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
      ...options?.meta,
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
  readonly labels: readonly backendModule.LabelName[] | null
  readonly sortExpression: backendModule.AssetSortExpression | null
  readonly sortDirection: backendModule.AssetSortDirection | null
  /**
   * When using React, use {@link useListDirectoryRefetchInterval} to get the correct value.
   * `undefined` is intentionally excluded as this value should be explicitly given.
   */
  readonly refetchInterval: number | null
  readonly infinite?: boolean
}

/** Build a query options object to fetch the children of a directory. */
export function listDirectoryQueryOptions(options: ListDirectoryQueryOptions) {
  const {
    backend,
    parentId,
    category,
    refetchInterval,
    labels,
    sortExpression,
    sortDirection,
    filterBy = CATEGORY_TO_FILTER_BY[category.type],
    infinite = false,
  } = options
  const rootPath = 'rootPath' in category ? category.rootPath : undefined
  return {
    meta: { persist: false },
    queryKey: [
      backend.type,
      'listDirectory',
      parentId,
      {
        rootPath,
        labels,
        sortExpression,
        sortDirection,
        filterBy,
        recentProjects: category.type === 'recent',
        infinite,
      },
    ],
    ...(refetchInterval != null ? { refetchInterval } : {}),
    queryFn: async (
      _context,
      { from, pageSize }: Pick<backendModule.ListDirectoryRequestParams, 'from' | 'pageSize'> = {
        from: null,
        pageSize: null,
      },
    ) => {
      try {
        return await backend.listDirectory(
          {
            parentId,
            rootPath,
            labels,
            sortExpression,
            sortDirection,
            filterBy,
            recentProjects: category.type === 'recent',
            from: from ?? null,
            pageSize: pageSize ?? null,
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
  } satisfies UnusedSkipTokenOptions<backendModule.ListDirectoryResponseBody>
}

/** Options for {@link searchDirectoryQueryOptions}. */
export interface SearchDirectoryQueryOptions {
  readonly backend: Backend
  readonly parentId: DirectoryId | null
  readonly query: string | null
  readonly title: string | null
  readonly description: string | null
  readonly type: string | null
  readonly extension: string | null
  readonly labels: readonly backendModule.LabelName[] | null
  readonly sortExpression: backendModule.AssetSortExpression | null
  readonly sortDirection: backendModule.AssetSortDirection | null
  readonly infinite?: boolean
}

/** Build a query options object to fetch the children of a directory. */
export function searchDirectoryQueryOptions(options: SearchDirectoryQueryOptions) {
  const { backend, infinite = false, ...rest } = options
  return {
    // Even though the default stale time is 0, we want to ensure that the query is not cached.
    staleTime: 0,
    meta: { persist: false },
    queryKey: ((): QueryKey => [backend.type, 'searchDirectory', { ...rest, infinite }])(),
    queryFn: (
      _context,
      { from, pageSize }: Pick<backendModule.SearchDirectoryRequestParams, 'from' | 'pageSize'> = {
        from: null,
        pageSize: null,
      },
    ) => backend.searchDirectory({ ...rest, from, pageSize }),
  } satisfies UnusedSkipTokenOptions<backendModule.ListDirectoryResponseBody>
}

/** Options for {@link unsafe_assetFromCacheQueryOptions}. */
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
          let data = query.state.data
          // Some queries store assets in infinite queries
          if (
            typeof data === 'object' &&
            data != null &&
            'pages' in data &&
            Array.isArray(data.pages)
          ) {
            data = data.pages.flatMap((page: unknown) =>
              typeof page === 'object' && page != null && 'assets' in page ? page.assets : [],
            )
          }
          // Some queries store assets arrays
          if (Array.isArray(data)) {
            // eslint-disable-next-line no-restricted-syntax
            const asset = data.find((maybeAsset) => assetSchema.safeParse(maybeAsset).success) as
              | AnyAsset
              | undefined
            if (asset != null) return asset
          }
          // And sometimes we store them directly
          const result = assetSchema.safeParse(data)
          // eslint-disable-next-line no-restricted-syntax
          if (result.success) return data as AnyAsset | undefined
          return null
        })
        .filter((asset) => asset != null)[0],
  })
}

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
      // We rely on mutation key pointing to properly typed mutation.
      // eslint-disable-next-line no-restricted-syntax
      predicate: ((mutation: BackendMutation<Method>) =>
        mutation.state.status === 'pending' && (predicate?.(mutation) ?? true)) as (
        mutation: Mutation,
      ) => boolean,
    },
    // This is UNSAFE when the `Result` parameter is explicitly specified in the
    // generic parameter list.
    // eslint-disable-next-line no-restricted-syntax
    select: select as (mutation: Mutation) => Result,
  })
}

/** Return query data for the children of a directory, fetching it if it does not exist. */
export function useEnsureListDirectory(backend: Backend, category: Category) {
  const queryClient = useQueryClient()
  return useEventCallback(async (parentId: DirectoryId) => {
    return (
      await queryClient.ensureQueryData(
        backendQueryOptions(backend, 'listDirectory', [
          {
            parentId,
            labels: null,
            filterBy: CATEGORY_TO_FILTER_BY[category.type],
            recentProjects: category.type === 'recent',
            sortExpression: null,
            sortDirection: null,
            from: null,
            pageSize: null,
          },
          '(unknown)',
        ]),
      )
    ).assets
  })
}

/** A function to create a new folder. */
export function useNewFolder(backend: Backend, category: Category) {
  const ensureListDirectory = useEnsureListDirectory(backend, category)
  const setNewestFolderId = useSetAssetToRename()
  const setSelectedAssets = useSetSelectedAssets()

  const createDirectory = useMutationCallback(backendMutationOptions(backend, 'createDirectory'))

  return useEventCallback(async (parentId: DirectoryId) => {
    const siblings = await ensureListDirectory(parentId)

    const directoryIndices = siblings
      .filter(backendModule.assetIsDirectory)
      .map((item) => /^New Folder (?<directoryIndex>\d+)$/.exec(item.title))
      .map((match) => match?.groups?.directoryIndex)
      .map((maybeIndex) => (maybeIndex != null ? parseInt(maybeIndex, 10) : 0))

    const title = `New Folder ${Math.max(0, ...directoryIndices) + 1}`

    return await createDirectory([{ parentId, title }]).then((result) => {
      setNewestFolderId(result.id)
      setSelectedAssets([{ type: AssetType.directory, ...result }])
      return result
    })
  })
}

/** A function to create a new project. */
export function useNewProject(backend: Backend, category: Category) {
  const ensureListDirectory = useEnsureListDirectory(backend, category)
  const { openProjectLocally } = useOpenedProjects()

  const createProject = useMutationCallback(backendMutationOptions(backend, 'createProject'))

  return useEventCallback(
    async (
      {
        templateName,
        ensoPath,
      }: {
        templateName?: string | null | undefined
        ensoPath?: string | null | undefined
      },
      parentId: DirectoryId,
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

      return await createProject([
        {
          parentDirectoryId: parentId,
          projectName,
          ...(ensoPath == null ? {} : { ensoPath }),
        },
      ]).then((createdProject) => {
        const openProjectParams = {
          id: createdProject.projectId,
          parentId: parentId,
          title: createdProject.name,
          ensoPath: createdProject.ensoPath,
        } satisfies Omit<ProjectInfo, 'mode'>
        openProjectLocally(openProjectParams, backend.type)
        return createdProject
      })
    },
  )
}

/** Remove the user's own permission from an asset. */
export function useRemoveSelfPermissionMutation(backend: Backend) {
  const { user } = useFullUserSession()

  const createPermission = useMutationCallback(
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
    void createPermission([
      {
        action: null,
        resourceId: id,
        actorsIds: [user.userId],
      },
    ])
  })

  const mutateAsync = useEventCallback(async (id: AssetId) => {
    await createPermission([
      {
        action: null,
        resourceId: id,
        actorsIds: [user.userId],
      },
    ])
  })

  return { ...createPermission, mutate, mutateAsync }
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

/** Return a function to rename an asset. */
export function useRenameAsset(backend: Backend) {
  const updateAsset = useMutationCallback(backendMutationOptions(backend, 'updateAsset'))

  return useEventCallback(
    (assetId: AssetId, newTitle: string, metadataId?: backendModule.MetadataId) => {
      return updateAsset([
        assetId,
        {
          title: newTitle,
          parentDirectoryId: null,
          description: null,
          metadataId: metadataId ?? null,
        },
        assetId,
      ])
    },
  )
}
