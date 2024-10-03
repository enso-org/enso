/** @file Hooks for interacting with the backend. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import * as backendQuery from 'enso-common/src/backendQuery'

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'
import invariant from 'tiny-invariant'

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
  // TODO: `get*` are not mutations, but are currently used in some places.
  | 'getDatalink'
  | 'getFileDetails'
  | 'getProjectDetails'
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

export function useBackendQuery<Method extends backendQuery.BackendMethods>(
  backend: Backend,
  method: Method,
  args: Parameters<Backend[Method]>,
  options?: Omit<
    reactQuery.UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>,
    'queryFn' | 'queryKey'
  > &
    Partial<Pick<reactQuery.UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryKey'>>,
): reactQuery.UseQueryResult<Awaited<ReturnType<Backend[Method]>>>
export function useBackendQuery<Method extends backendQuery.BackendMethods>(
  backend: Backend | null,
  method: Method,
  args: Parameters<Backend[Method]>,
  options?: Omit<
    reactQuery.UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>,
    'queryFn' | 'queryKey'
  > &
    Partial<Pick<reactQuery.UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryKey'>>,
): reactQuery.UseQueryResult<
  // eslint-disable-next-line no-restricted-syntax
  Awaited<ReturnType<Backend[Method]>> | undefined
>
/** Wrap a backend method call in a React Query. */
export function useBackendQuery<Method extends backendQuery.BackendMethods>(
  backend: Backend | null,
  method: Method,
  args: Parameters<Backend[Method]>,
  options?: Omit<
    reactQuery.UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>,
    'queryFn' | 'queryKey'
  > &
    Partial<Pick<reactQuery.UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryKey'>>,
) {
  return reactQuery.useQuery<Awaited<ReturnType<Backend[Method]>>>({
    ...options,
    ...backendQuery.backendQueryOptions(backend, method, args, options?.queryKey),
    // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-return
    queryFn: () => (backend?.[method] as any)?.(...args),
  })
}

// ==========================
// === useBackendMutation ===
// ==========================

const INVALIDATE_ALL_QUERIES = Symbol('invalidate all queries')
const INVALIDATION_MAP: Partial<
  Record<MutationMethod, readonly (backendQuery.BackendMethods | typeof INVALIDATE_ALL_QUERIES)[]>
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
  createDatalink: ['listDirectory'],
  uploadFile: ['listDirectory'],
  copyAsset: ['listDirectory', 'listAssetVersions'],
  deleteAsset: ['listDirectory', 'listAssetVersions'],
  undoDeleteAsset: ['listDirectory'],
  updateAsset: ['listDirectory', 'listAssetVersions'],
  closeProject: ['listDirectory', 'listAssetVersions'],
  updateDirectory: ['listDirectory'],
}

export function backendMutationOptions<Method extends MutationMethod>(
  backend: Backend,
  method: Method,
  options?: Omit<
    reactQuery.UseMutationOptions<
      Awaited<ReturnType<Backend[Method]>>,
      Error,
      Parameters<Backend[Method]>
    >,
    'mutationFn'
  >,
): reactQuery.UseMutationOptions<
  Awaited<ReturnType<Backend[Method]>>,
  Error,
  Parameters<Backend[Method]>
>
export function backendMutationOptions<Method extends MutationMethod>(
  backend: Backend | null,
  method: Method,
  options?: Omit<
    reactQuery.UseMutationOptions<
      Awaited<ReturnType<Backend[Method]>>,
      Error,
      Parameters<Backend[Method]>
    >,
    'mutationFn'
  >,
): reactQuery.UseMutationOptions<
  Awaited<ReturnType<Backend[Method]>> | undefined,
  Error,
  Parameters<Backend[Method]>
>
/** Wrap a backend method call in a React Query Mutation. */
export function backendMutationOptions<Method extends MutationMethod>(
  backend: Backend | null,
  method: Method,
  options?: Omit<
    reactQuery.UseMutationOptions<
      Awaited<ReturnType<Backend[Method]>>,
      Error,
      Parameters<Backend[Method]>
    >,
    'mutationFn'
  >,
): reactQuery.UseMutationOptions<
  Awaited<ReturnType<Backend[Method]>>,
  Error,
  Parameters<Backend[Method]>
> {
  return {
    ...options,
    mutationKey: [backend?.type, method, ...(options?.mutationKey ?? [])],
    // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-return
    mutationFn: (args) => (backend?.[method] as any)?.(...args),
    networkMode: backend?.type === backendModule.BackendType.local ? 'always' : 'online',
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
export interface UserGroupInfoWithUsers extends backendModule.UserGroupInfo {
  readonly users: readonly backendModule.User[]
}

/** A list of user groups, taking into account optimistic state. */
export function useListUserGroupsWithUsers(
  backend: Backend,
): readonly UserGroupInfoWithUsers[] | null {
  const listUserGroupsQuery = useBackendQuery(backend, 'listUserGroups', [])
  const listUsersQuery = useBackendQuery(backend, 'listUsers', [])
  return React.useMemo(() => {
    if (listUserGroupsQuery.data == null || listUsersQuery.data == null) {
      return null
    } else {
      const result = listUserGroupsQuery.data.map((userGroup) => {
        const usersInGroup: readonly backendModule.User[] = listUsersQuery.data.filter((user) =>
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
  backendType: backendModule.BackendType,
  assetId: backendModule.AssetId | null | undefined,
  parentId: backendModule.DirectoryId | null | undefined,
) {
  const queryClient = reactQuery.useQueryClient()
  const listDirectoryQuery = queryClient.getQueryCache().find<
    | {
        parentId: backendModule.DirectoryId
        children: readonly backendModule.AnyAsset<backendModule.AssetType>[]
      }
    | undefined
  >({
    queryKey: [backendType, 'listDirectory', parentId],
    exact: false,
  })

  return listDirectoryQuery?.state.data?.children.find((child) => child.id === assetId)
}

/** Data for a specific asset. */
export function useAssetPassiveListenerStrict(
  backendType: backendModule.BackendType,
  assetId: backendModule.AssetId | null | undefined,
  parentId: backendModule.DirectoryId | null | undefined,
) {
  const queryClient = reactQuery.useQueryClient()
  const listDirectoryQuery = queryClient.getQueryCache().find<
    | {
        parentId: backendModule.DirectoryId
        children: readonly backendModule.AnyAsset<backendModule.AssetType>[]
      }
    | undefined
  >({
    queryKey: [backendType, 'listDirectory', parentId],
    exact: false,
  })

  const asset = listDirectoryQuery?.state.data?.children.find((child) => child.id === assetId)
  invariant(asset, 'Asset not found')
  return asset
}

/** Return a hook to set data for a specific asset. */
export function useSetAsset(backendType: backendModule.BackendType) {
  const queryClient = reactQuery.useQueryClient()
  return useEventCallback((assetId: backendModule.AssetId, asset: backendModule.AnyAsset) => {
    const listDirectoryQuery = queryClient.getQueryCache().find<
      | {
          parentId: backendModule.DirectoryId
          children: readonly backendModule.AnyAsset<backendModule.AssetType>[]
        }
      | undefined
    >({
      queryKey: [backendType, 'listDirectory', asset.parentId],
      exact: false,
    })

    if (listDirectoryQuery?.state.data) {
      listDirectoryQuery.setData({
        ...listDirectoryQuery.state.data,
        children: listDirectoryQuery.state.data.children.map((child) =>
          child.id === assetId ? asset : child,
        ),
      })
    }
  })
}
