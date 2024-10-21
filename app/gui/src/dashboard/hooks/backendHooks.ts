/** @file Hooks for interacting with the backend. */
import { useMemo } from 'react'

import {
  queryOptions,
  useMutationState,
  useQuery,
  type Mutation,
  type MutationKey,
  type UseMutationOptions,
  type UseQueryOptions,
  type UseQueryResult,
} from '@tanstack/react-query'
import invariant from 'tiny-invariant'

import {
  backendQueryOptions as backendQueryOptionsBase,
  type BackendMethods,
} from 'enso-common/src/backendQuery'

import { CATEGORY_TO_FILTER_BY, type Category } from '#/layouts/CategorySwitcher/Category'
import type Backend from '#/services/Backend'
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

// ============================
// === DefineBackendMethods ===
// ============================

/** Ensure that the given type contains only names of backend methods. */
// eslint-disable-next-line no-restricted-syntax
type DefineBackendMethods<T extends keyof Backend> = T

// ======================
// === MutationMethod ===
// ======================

/** Names of methods corresponding to mutations. */
export type MutationMethod = DefineBackendMethods<
  | 'acceptInvitation'
  | 'associateTag'
  | 'changeUserGroup'
  | 'closeProject'
  | 'copyAsset'
  | 'createCheckoutSession'
  | 'createDatalink'
  | 'createDirectory'
  | 'createPermission'
  | 'createProject'
  | 'createSecret'
  | 'createTag'
  | 'createUser'
  | 'createUserGroup'
  | 'declineInvitation'
  | 'deleteAsset'
  | 'deleteDatalink'
  | 'deleteInvitation'
  | 'deleteTag'
  | 'deleteUser'
  | 'deleteUserGroup'
  | 'duplicateProject'
  | 'inviteUser'
  | 'logEvent'
  | 'openProject'
  | 'removeUser'
  | 'resendInvitation'
  | 'restoreUser'
  | 'undoDeleteAsset'
  | 'updateAsset'
  | 'updateDirectory'
  | 'updateFile'
  | 'updateOrganization'
  | 'updateProject'
  | 'updateSecret'
  | 'updateUser'
  | 'uploadFile'
  | 'uploadOrganizationPicture'
  | 'uploadUserPicture'
>

// =======================
// === useBackendQuery ===
// =======================

export function backendQueryOptions<Method extends BackendMethods>(
  backend: Backend,
  method: Method,
  args: Parameters<Backend[Method]>,
  options?: Omit<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryFn' | 'queryKey'> &
    Partial<Pick<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryKey'>>,
): UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>
export function backendQueryOptions<Method extends BackendMethods>(
  backend: Backend | null,
  method: Method,
  args: Parameters<Backend[Method]>,
  options?: Omit<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryFn' | 'queryKey'> &
    Partial<Pick<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryKey'>>,
): UseQueryOptions<
  // eslint-disable-next-line no-restricted-syntax
  Awaited<ReturnType<Backend[Method]>> | undefined
>
/** Wrap a backend method call in a React Query. */
export function backendQueryOptions<Method extends BackendMethods>(
  backend: Backend | null,
  method: Method,
  args: Parameters<Backend[Method]>,
  options?: Omit<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryFn' | 'queryKey'> &
    Partial<Pick<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryKey'>>,
) {
  // @ts-expect-error This call is generic over the presence or absence of `inputData`.
  return queryOptions<Awaited<ReturnType<Backend[Method]>>>({
    ...options,
    ...backendQueryOptionsBase(backend, method, args, options?.queryKey),
    // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-return
    queryFn: () => (backend?.[method] as any)?.(...args),
  })
}

export function useBackendQuery<Method extends BackendMethods>(
  backend: Backend,
  method: Method,
  args: Parameters<Backend[Method]>,
  options?: Omit<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryFn' | 'queryKey'> &
    Partial<Pick<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryKey'>>,
): UseQueryResult<Awaited<ReturnType<Backend[Method]>>>
export function useBackendQuery<Method extends BackendMethods>(
  backend: Backend | null,
  method: Method,
  args: Parameters<Backend[Method]>,
  options?: Omit<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryFn' | 'queryKey'> &
    Partial<Pick<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryKey'>>,
): UseQueryResult<
  // eslint-disable-next-line no-restricted-syntax
  Awaited<ReturnType<Backend[Method]>> | undefined
>
/** Wrap a backend method call in a React Query. */
export function useBackendQuery<Method extends BackendMethods>(
  backend: Backend | null,
  method: Method,
  args: Parameters<Backend[Method]>,
  options?: Omit<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryFn' | 'queryKey'> &
    Partial<Pick<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryKey'>>,
) {
  return useQuery(backendQueryOptions(backend, method, args, options))
}

// ==========================
// === useBackendMutation ===
// ==========================

const INVALIDATE_ALL_QUERIES = Symbol('invalidate all queries')
const INVALIDATION_MAP: Partial<
  Record<MutationMethod, readonly (BackendMethods | typeof INVALIDATE_ALL_QUERIES)[]>
> = {
  createUser: ['usersMe'],
  updateUser: ['usersMe'],
  deleteUser: ['usersMe'],
  restoreUser: ['usersMe'],
  uploadUserPicture: ['usersMe'],
  updateOrganization: ['getOrganization'],
  uploadOrganizationPicture: ['getOrganization'],
  createUserGroup: ['listUserGroups'],
  deleteUserGroup: ['listUserGroups'],
  changeUserGroup: ['listUsers'],
  createTag: ['listTags'],
  deleteTag: ['listTags'],
  associateTag: ['listDirectory'],
  acceptInvitation: [INVALIDATE_ALL_QUERIES],
  declineInvitation: ['usersMe'],
  createProject: ['listDirectory'],
  duplicateProject: ['listDirectory'],
  createDirectory: ['listDirectory'],
  createSecret: ['listDirectory'],
  updateSecret: ['listDirectory'],
  createDatalink: ['listDirectory', 'getDatalink'],
  uploadFile: ['listDirectory'],
  copyAsset: ['listDirectory', 'listAssetVersions'],
  deleteAsset: ['listDirectory', 'listAssetVersions'],
  undoDeleteAsset: ['listDirectory'],
  updateAsset: ['listDirectory', 'listAssetVersions'],
  closeProject: ['listDirectory', 'listAssetVersions'],
  updateDirectory: ['listDirectory'],
}

/** The type of the corresponding mutation for the given backend method. */
export type BackendMutation<Method extends MutationMethod> = Mutation<
  Awaited<ReturnType<Backend[Method]>>,
  Error,
  Parameters<Backend[Method]>
>

export function backendMutationOptions<Method extends MutationMethod>(
  backend: Backend,
  method: Method,
  options?: Omit<
    UseMutationOptions<Awaited<ReturnType<Backend[Method]>>, Error, Parameters<Backend[Method]>>,
    'mutationFn'
  >,
): UseMutationOptions<Awaited<ReturnType<Backend[Method]>>, Error, Parameters<Backend[Method]>>
export function backendMutationOptions<Method extends MutationMethod>(
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
export function backendMutationOptions<Method extends MutationMethod>(
  backend: Backend | null,
  method: Method,
  options?: Omit<
    UseMutationOptions<Awaited<ReturnType<Backend[Method]>>, Error, Parameters<Backend[Method]>>,
    'mutationFn'
  >,
): UseMutationOptions<Awaited<ReturnType<Backend[Method]>>, Error, Parameters<Backend[Method]>> {
  return {
    ...options,
    mutationKey: [backend?.type, method, ...(options?.mutationKey ?? [])],
    // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-return
    mutationFn: (args) => (backend?.[method] as any)?.(...args),
    networkMode: backend?.type === BackendType.local ? 'always' : 'online',
    meta: {
      invalidates: [
        ...(options?.meta?.invalidates ?? []),
        ...(INVALIDATION_MAP[method]?.map((queryMethod) =>
          queryMethod === INVALIDATE_ALL_QUERIES ? [backend?.type] : [backend?.type, queryMethod],
        ) ?? []),
      ],
      awaitInvalidates: options?.meta?.awaitInvalidates ?? true,
    },
  }
}

// ==================================
// === useListUserGroupsWithUsers ===
// ==================================

/** A user group, as well as the users that are a part of the user group. */
export interface UserGroupInfoWithUsers extends UserGroupInfo {
  readonly users: readonly User[]
}

/** A list of user groups, taking into account optimistic state. */
export function useListUserGroupsWithUsers(
  backend: Backend,
): readonly UserGroupInfoWithUsers[] | null {
  const listUserGroupsQuery = useBackendQuery(backend, 'listUserGroups', [])
  const listUsersQuery = useBackendQuery(backend, 'listUsers', [])
  return useMemo(() => {
    if (listUserGroupsQuery.data == null || listUsersQuery.data == null) {
      return null
    } else {
      const result = listUserGroupsQuery.data.map((userGroup) => {
        const usersInGroup: readonly User[] = listUsersQuery.data.filter((user) =>
          user.userGroups?.includes(userGroup.id),
        )
        return { ...userGroup, users: usersInGroup }
      })
      return result
    }
  }, [listUserGroupsQuery.data, listUsersQuery.data])
}

/** Data for a specific asset. */
export function useAssetPassiveListener(
  backendType: BackendType,
  assetId: AssetId | null | undefined,
  parentId: DirectoryId | null | undefined,
  category: Category,
) {
  const listDirectoryQuery = useQuery<readonly AnyAsset<AssetType>[] | undefined>({
    queryKey: [
      backendType,
      'listDirectory',
      parentId,
      {
        labels: null,
        filterBy: CATEGORY_TO_FILTER_BY[category.type],
        recentProjects: category.type === 'recent',
      },
    ],
    initialData: undefined,
  })
  const asset = listDirectoryQuery.data?.find((child) => child.id === assetId)
  if (asset || !assetId || !parentId) {
    return asset
  }
  switch (assetId) {
    case USERS_DIRECTORY_ID: {
      return {
        id: assetId,
        parentId,
        type: AssetType.directory,
        projectState: null,
        title: 'Users',
        description: '',
        modifiedAt: toRfc3339(new Date()),
        permissions: [],
        labels: [],
      } satisfies DirectoryAsset
    }
    case TEAMS_DIRECTORY_ID: {
      return {
        id: assetId,
        parentId,
        type: AssetType.directory,
        projectState: null,
        title: 'Teams',
        description: '',
        modifiedAt: toRfc3339(new Date()),
        permissions: [],
        labels: [],
      } satisfies DirectoryAsset
    }
    default: {
      return
    }
  }
}

/** Data for a specific asset. */
export function useAssetPassiveListenerStrict(
  backendType: BackendType,
  assetId: AssetId | null | undefined,
  parentId: DirectoryId | null | undefined,
  category: Category,
) {
  const asset = useAssetPassiveListener(backendType, assetId, parentId, category)
  invariant(asset, 'Asset not found')
  return asset
}

/** Return matching in-flight mutations */
export function useBackendMutationState<Method extends MutationMethod, Result>(
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
