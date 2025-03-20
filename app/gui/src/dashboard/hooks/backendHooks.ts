/** @file Hooks for interacting with the backend. */
import {
  queryOptions,
  useMutation,
  useMutationState,
  useQuery,
  useQueryClient,
  useSuspenseQuery,
  type DefaultError,
  type Mutation,
  type MutationKey,
  type QueryClient,
  type QueryKey,
  type UnusedSkipTokenOptions,
  type UseMutationOptions,
  type UseQueryOptions,
  type UseQueryResult,
} from '@tanstack/react-query'
import invariant from 'tiny-invariant'

import {
  backendQueryOptions as backendQueryOptionsBase,
  INVALIDATE_ALL_QUERIES,
  INVALIDATION_MAP,
  type BackendMutationMethod,
  type BackendQueryMethod,
} from 'enso-common/src/backendQuery'

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useOpenProject } from '#/hooks/projectHooks'
import { CATEGORY_TO_FILTER_BY, type Category } from '#/layouts/CategorySwitcher/Category'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useSetNewestFolderId, useSetSelectedAssets } from '#/providers/DriveProvider'
import { useLocalStorageState } from '#/providers/LocalStorageProvider'
import type { LaunchedProject } from '#/providers/ProjectsProvider'
import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'
import {
  AssetType,
  BackendType,
  type AnyAsset,
  type AssetId,
  type DirectoryAsset,
  type DirectoryId,
  type User,
  type UserGroupInfo,
} from '#/services/Backend'
import { TEAMS_DIRECTORY_ID, USERS_DIRECTORY_ID } from '#/services/remoteBackendPaths'
import { toRfc3339 } from 'enso-common/src/utilities/data/dateTime'
import type { MergeValuesOfObjectUnion } from 'enso-common/src/utilities/data/object'
import { useMemo } from 'react'
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
    // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-return
    queryFn: () => (backend?.[method] as any)?.(...args),
  })
}

/** An identity function to help in constructing options for a mutation. */
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

export function useBackendQuery<Method extends BackendQueryMethod>(
  backend: Backend,
  method: Method,
  args: Readonly<Parameters<Backend[Method]>>,
  options?: Omit<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryFn' | 'queryKey'> &
    Partial<Pick<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryKey'>>,
): UseQueryResult<Awaited<ReturnType<Backend[Method]>>>
export function useBackendQuery<Method extends BackendQueryMethod>(
  backend: Backend | null,
  method: Method,
  args: Readonly<Parameters<Backend[Method]>>,
  options?: Omit<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryFn' | 'queryKey'> &
    Partial<Pick<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryKey'>>,
): UseQueryResult<Awaited<ReturnType<Backend[Method]>> | undefined>
/** Wrap a backend method call in a React Query. */
export function useBackendQuery<Method extends BackendQueryMethod>(
  backend: Backend | null,
  method: Method,
  args: Readonly<Parameters<Backend[Method]>>,
  options?: Omit<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryFn' | 'queryKey'> &
    Partial<Pick<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryKey'>>,
) {
  return useQuery(backendQueryOptions(backend, method, args, options))
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

/** Create a list of {@link UserGroupInfoWithUsers} given user groups and users. */
function createUserGroupsWithUsers(
  userGroups: readonly backendModule.UserGroupInfo[],
  users: readonly backendModule.User[],
): readonly UserGroupInfoWithUsers[] {
  return userGroups.map((userGroup) => {
    const usersInGroup: readonly User[] = users.filter(
      (user) => user.userGroups?.includes(userGroup.id) ?? false,
    )
    return { ...userGroup, users: usersInGroup }
  })
}

/** The return value of {@link useListUserGroupsWithUsers}. */
export type ListUserGroupsWithUsersQueryResult = Omit<
  UseQueryResult<readonly UserGroupInfoWithUsers[]>,
  'refetch'
>

/** A list of user groups, taking into account optimistic state. */
export function useListUserGroupsWithUsers(backend: Backend): ListUserGroupsWithUsersQueryResult {
  const listUserGroupsQuery = useBackendQuery(backend, 'listUserGroups', [])
  const listUsersQuery = useBackendQuery(backend, 'listUsers', [])

  const promise: Promise<readonly UserGroupInfoWithUsers[]> = useMemo(
    () =>
      Promise.all([listUsersQuery.promise, listUserGroupsQuery.promise]).then(
        ([users, userGroups]) => createUserGroupsWithUsers(userGroups, users),
      ),
    [listUserGroupsQuery.promise, listUsersQuery.promise],
  )

  const error = listUserGroupsQuery.error ?? listUsersQuery.error
  const isStale = listUsersQuery.isStale || listUserGroupsQuery.isStale
  const failureCount = listUsersQuery.failureCount + listUserGroupsQuery.failureCount
  const failureReason = listUserGroupsQuery.failureReason ?? listUsersQuery.failureReason
  const dataUpdatedAt = Math.max(listUsersQuery.dataUpdatedAt, listUserGroupsQuery.dataUpdatedAt)
  const errorUpdatedAt = Math.max(listUsersQuery.errorUpdatedAt, listUserGroupsQuery.errorUpdatedAt)
  const errorUpdateCount = listUsersQuery.errorUpdateCount + listUserGroupsQuery.errorUpdateCount
  const isFetched = listUsersQuery.isFetched && listUserGroupsQuery.isFetched
  const isFetching = listUsersQuery.isFetching || listUserGroupsQuery.isFetching
  const isPaused = listUsersQuery.isPaused || listUserGroupsQuery.isPaused
  const isInitialLoading = listUsersQuery.isInitialLoading || listUserGroupsQuery.isInitialLoading
  const isFetchedAfterMount =
    listUsersQuery.isFetchedAfterMount || listUserGroupsQuery.isFetchedAfterMount
  const isPlaceholderData =
    listUsersQuery.isPlaceholderData || listUserGroupsQuery.isPlaceholderData
  const isRefetching = listUsersQuery.isRefetching || listUserGroupsQuery.isRefetching
  const fetchStatus = (() => {
    if (listUsersQuery.isPaused || listUserGroupsQuery.isPaused) {
      return 'paused'
    }
    if (listUsersQuery.isFetching || listUserGroupsQuery.isFetching) {
      return 'fetching'
    }
    return 'idle'
  })()
  const status = (() => {
    if (listUsersQuery.isSuccess && listUserGroupsQuery.isSuccess) {
      return 'success'
    }
    if (listUsersQuery.isError || listUserGroupsQuery.isError) {
      return 'error'
    }
    return 'pending'
  })()
  const shared = {
    promise,
    error,
    isStale,
    failureCount,
    failureReason,
    dataUpdatedAt,
    errorUpdatedAt,
    errorUpdateCount,
    isFetched,
    isFetching,
    isPaused,
    isInitialLoading,
    isFetchedAfterMount,
    isPlaceholderData,
    isRefetching,
    fetchStatus,
    status,
  } satisfies Partial<MergeValuesOfObjectUnion<ListUserGroupsWithUsersQueryResult>>

  if (listUsersQuery.isSuccess && listUserGroupsQuery.isSuccess) {
    const data = createUserGroupsWithUsers(listUserGroupsQuery.data, listUsersQuery.data)
    const rest = {
      data,
      isSuccess: true,
      isError: false,
      isPending: false,
      isLoading: false,
      isLoadingError: false,
      isRefetchError: false,
      status: 'success',
      error: null,
    } satisfies Partial<ListUserGroupsWithUsersQueryResult>
    return {
      // This is UNSAFE. Care must be taken to ensure that states are merged correctly in `shared`.
      // eslint-disable-next-line no-restricted-syntax
      ...(shared as Omit<ListUserGroupsWithUsersQueryResult, keyof typeof rest>),
      ...rest,
    }
  } else {
    // This is UNSAFE. Care must be taken to ensure that states are merged correctly in `shared`.
    // eslint-disable-next-line no-restricted-syntax
    return shared as ListUserGroupsWithUsersQueryResult
  }
}

/** Options for {@link listDirectoryQueryOptions}. */
export interface ListDirectoryQueryOptions {
  readonly backend: Backend
  readonly parentId: DirectoryId
  readonly category: Category
}

/** Build a query options object to fetch the children of a directory. */
export function listDirectoryQueryOptions(options: ListDirectoryQueryOptions) {
  const { backend, parentId, category } = options

  const rootPath = 'rootPath' in category ? category.rootPath : undefined

  return queryOptions({
    queryKey: [
      backend.type,
      'listDirectory',
      parentId,
      {
        rootPath,
        labels: null,
        filterBy: CATEGORY_TO_FILTER_BY[category.type],
        recentProjects: category.type === 'recent',
      },
    ] as const,
    // Setting stale time to `Infinity` avoids attaching a ton of
    // setTimeouts to the query. Improves performance.
    // This is fine as refetching is handled by another query.
    staleTime: Infinity,
    queryFn: async () => {
      try {
        return await backend.listDirectory(
          {
            parentId,
            rootPath,
            filterBy: CATEGORY_TO_FILTER_BY[category.type],
            labels: null,
            recentProjects: category.type === 'recent',
          },
          parentId,
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

/** Options for {@link useAsset}. */
export interface UseAssetOptions extends ListDirectoryQueryOptions {
  readonly assetId: AssetId
}

/** Data for a specific asset. */
export function useAsset(options: UseAssetOptions) {
  const { parentId, assetId } = options

  const { data: asset } = useQuery({
    ...listDirectoryQueryOptions(options),
    select: (data) => data.find((child) => child.id === assetId),
  })

  if (asset) {
    return asset
  }

  const shared = {
    parentId,
    projectState: null,
    extension: null,
    description: '',
    modifiedAt: toRfc3339(new Date()),
    permissions: [],
    labels: [],
    parentsPath: backendModule.ParentsPath(''),
    virtualParentsPath: backendModule.VirtualParentsPath(''),
  } satisfies Partial<DirectoryAsset>
  // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
  switch (true) {
    case assetId === USERS_DIRECTORY_ID: {
      return {
        ...shared,
        id: assetId,
        title: 'Users',
        type: AssetType.directory,
      } satisfies DirectoryAsset
    }
    case assetId === TEAMS_DIRECTORY_ID: {
      return {
        ...shared,
        id: assetId,
        title: 'Teams',
        type: AssetType.directory,
      } satisfies DirectoryAsset
    }
    case backendModule.isLoadingAssetId(assetId): {
      return {
        ...shared,
        id: assetId,
        title: '',
        type: AssetType.specialLoading,
      } satisfies backendModule.SpecialLoadingAsset
    }
    case backendModule.isEmptyAssetId(assetId): {
      return {
        ...shared,
        id: assetId,
        title: '',
        type: AssetType.specialEmpty,
      } satisfies backendModule.SpecialEmptyAsset
    }
    case backendModule.isErrorAssetId(assetId): {
      return {
        ...shared,
        id: assetId,
        title: '',
        type: AssetType.specialError,
      } satisfies backendModule.SpecialErrorAsset
    }
    default: {
      return
    }
  }
}

/** Non-nullable for a specific asset. */
export function useAssetStrict(options: UseAssetOptions) {
  const asset = useAsset(options)

  invariant(
    asset,
    `Expected asset to be defined, but got undefined, Asset ID: ${JSON.stringify(options.assetId)}`,
  )

  return asset
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
      predicate: (mutation: BackendMutation<Method>) =>
        mutation.state.status === 'pending' && (predicate?.(mutation) ?? true),
    },
    // This is UNSAFE when the `Result` parameter is explicitly specified in the
    // generic parameter list.
    // eslint-disable-next-line no-restricted-syntax
    select: select as (mutation: Mutation<unknown, Error, unknown, unknown>) => Result,
  })
}

/** Get the root directory ID given the current backend and category. */
export function useRootDirectoryId(backend: Backend, category: Category) {
  const { user } = useFullUserSession()
  const { data: organization } = useSuspenseQuery({
    queryKey: [backend.type, 'getOrganization'],
    queryFn: () => backend.getOrganization(),
  })
  const [localRootDirectory] = useLocalStorageState('localRootDirectory')

  const localRootPath = localRootDirectory != null ? backendModule.Path(localRootDirectory) : null
  const id =
    'homeDirectoryId' in category ?
      category.homeDirectoryId
    : backend.rootDirectoryId(user, organization, localRootPath)
  invariant(id, 'Missing root directory')
  return id
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
  const createDirectoryMutation = useMutation(backendMutationOptions(backend, 'createDirectory'))

  return useEventCallback(async (parentId: DirectoryId) => {
    const siblings = await ensureListDirectory(parentId)

    const directoryIndices = siblings
      .filter(backendModule.assetIsDirectory)
      .map((item) => /^New Folder (?<directoryIndex>\d+)$/.exec(item.title))
      .map((match) => match?.groups?.directoryIndex)
      .map((maybeIndex) => (maybeIndex != null ? parseInt(maybeIndex, 10) : 0))

    const title = `New Folder ${Math.max(0, ...directoryIndices) + 1}`
    const placeholderItem = backendModule.createPlaceholderDirectoryAsset(title, parentId)

    return await createDirectoryMutation
      .mutateAsync([{ parentId: placeholderItem.parentId, title: placeholderItem.title }])
      .then((result) => {
        setNewestFolderId(result.id)
        setSelectedAssets([{ type: AssetType.directory, ...result }])
        return result
      })
  })
}

/** A function to create a new project. */
export function useNewProject(backend: Backend, category: Category) {
  const ensureListDirectory = useEnsureListDirectory(backend, category)
  const doOpenProject = useOpenProject()
  const deleteAsset = useDeleteAsset(backend, category)

  const createProjectMutation = useMutation(backendMutationOptions(backend, 'createProject'))

  return useEventCallback(
    async (
      {
        templateName,
        templateId,
        datalinkId,
      }: {
        templateName: string | null | undefined
        templateId?: string | null | undefined
        datalinkId?: backendModule.DatalinkId | null | undefined
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

      const placeholderItem = backendModule.createPlaceholderProjectAsset(projectName, parentId)

      return await createProjectMutation
        .mutateAsync([
          {
            parentDirectoryId: placeholderItem.parentId,
            projectName: placeholderItem.title,
            ...(templateId == null ? {} : { projectTemplateName: templateId }),
            ...(datalinkId == null ? {} : { datalinkId: datalinkId }),
          },
        ])
        .catch((error) => {
          void deleteAsset(placeholderItem.id, parentId)
          throw error
        })
        .then((createdProject) => {
          doOpenProject({
            id: createdProject.projectId,
            type: backend.type,
            parentId: placeholderItem.parentId,
            title: createdProject.name,
          })

          return createdProject
        })
    },
  )
}

/** A function to create a new secret. */
export function useNewSecret(backend: Backend) {
  const createSecretMutation = useMutation(backendMutationOptions(backend, 'createSecret'))

  return useEventCallback(async (name: string, value: string, parentId: DirectoryId) => {
    const placeholderItem = backendModule.createPlaceholderSecretAsset(name, parentId)

    return await createSecretMutation.mutateAsync([
      {
        parentDirectoryId: placeholderItem.parentId,
        name: placeholderItem.title,
        value: value,
      },
    ])
  })
}

/** A function to create a new Datalink. */
export function useNewDatalink(backend: Backend) {
  const createDatalinkMutation = useMutation(backendMutationOptions(backend, 'createDatalink'))

  return useEventCallback(async (name: string, value: unknown, parentId: DirectoryId) => {
    const placeholderItem = backendModule.createPlaceholderDatalinkAsset(name, parentId)

    return await createDatalinkMutation.mutateAsync([
      {
        parentDirectoryId: placeholderItem.parentId,
        datalinkId: null,
        name: placeholderItem.title,
        value,
      },
    ])
  })
}

/** Remove the user's own permission from an asset. */
export function useRemoveSelfPermissionMutation(backend: Backend) {
  const { user } = useFullUserSession()

  const createPermissionMutation = useMutation(
    backendMutationOptions(backend, 'createPermission', {
      meta: {
        invalidates: [[backend.type, 'listDirectory']],
        awaitInvalidates: true,
      },
    }),
  )

  const mutate = useEventCallback((id: AssetId) => {
    createPermissionMutation.mutate([
      {
        action: null,
        resourceId: id,
        actorsIds: [user.userId],
      },
    ])
  })

  const mutateAsync = useEventCallback(async (id: AssetId) => {
    await createPermissionMutation.mutateAsync([
      {
        action: null,
        resourceId: id,
        actorsIds: [user.userId],
      },
    ])
  })

  return { ...createPermissionMutation, mutate, mutateAsync }
}

/** Duplicate a specific version of a project. */
export function duplicateProjectMutationOptions(
  backend: Backend,
  queryClient: QueryClient,
  openProject: (project: LaunchedProject) => void,
) {
  return mutationOptions({
    mutationFn: async ([id, originalTitle, parentId, versionId]: [
      id: backendModule.ProjectId,
      originalTitle: string,
      parentId: backendModule.DirectoryId,
      versionId: backendModule.S3ObjectVersionId,
    ]) => {
      const siblings = await queryClient.ensureQueryData(
        backendQueryOptions(backend, 'listDirectory', [
          {
            parentId,
            labels: null,
            filterBy: backendModule.FilterBy.active,
            recentProjects: false,
          },
          '(unknown)',
        ]),
      )
      const siblingTitles = new Set(siblings.map((sibling) => sibling.title))
      let index = 1
      let title = `${originalTitle} (${index})`
      while (siblingTitles.has(title)) {
        index += 1
        title = `${originalTitle} (${index})`
      }

      await backend.duplicateProject(id, versionId, title).then((project) => {
        openProject({
          type: backend.type,
          parentId,
          title,
          id: project.projectId,
        })
      })
    },
  })
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
