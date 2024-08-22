/** @file Hooks for interacting with the backend. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import * as backendQuery from 'enso-common/src/backendQuery'

import * as authProvider from '#/providers/AuthProvider'

import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'

import * as uniqueString from '#/utilities/uniqueString'

// ============================
// === revokeUserPictureUrl ===
// ============================

const USER_PICTURE_URLS = new Map<backendModule.BackendType, string>()

/** Create the corresponding "user picture" URL for the given backend. */
function createUserPictureUrl(
  backendType: backendModule.BackendType | null | undefined,
  picture: Blob,
) {
  if (backendType != null) {
    revokeUserPictureUrl(backendType)
    const url = URL.createObjectURL(picture)
    USER_PICTURE_URLS.set(backendType, url)
    return url
  } else {
    // This should never happen, so use an arbitrary URL.
    return location.href
  }
}

/** Revoke the corresponding "user picture" URL for the given backend. */
function revokeUserPictureUrl(backendType: backendModule.BackendType | null | undefined) {
  if (backendType != null) {
    const url = USER_PICTURE_URLS.get(backendType)
    if (url != null) {
      URL.revokeObjectURL(url)
    }
  }
}

// ====================================
// === revokeOrganizationPictureUrl ===
// ====================================

const ORGANIZATION_PICTURE_URLS = new Map<backendModule.BackendType, string>()

/** Create the corresponding "organization picture" URL for the given backend. */
function createOrganizationPictureUrl(
  backendType: backendModule.BackendType | null | undefined,
  picture: Blob,
) {
  if (backendType != null) {
    revokeOrganizationPictureUrl(backendType)
    const url = URL.createObjectURL(picture)
    ORGANIZATION_PICTURE_URLS.set(backendType, url)
    return url
  } else {
    // This should never happen, so use an arbitrary URL.
    return location.href
  }
}

/** Revoke the corresponding "organization picture" URL for the given backend. */
function revokeOrganizationPictureUrl(backendType: backendModule.BackendType | null | undefined) {
  if (backendType != null) {
    const url = ORGANIZATION_PICTURE_URLS.get(backendType)
    if (url != null) {
      URL.revokeObjectURL(url)
    }
  }
}

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
  | 'associateTag'
  | 'changeUserGroup'
  | 'closeProject'
  | 'copyAsset'
  | 'createCheckoutSession'
  | 'createDatalink'
  | 'createDirectory'
  | 'createPermission'
  | 'createProject'
  | 'createProjectExecution'
  | 'createSecret'
  | 'createTag'
  | 'createUser'
  | 'createUserGroup'
  | 'deleteAsset'
  | 'deleteDatalink'
  | 'deleteInvitation'
  | 'deleteProjectExecution'
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
  | 'syncProjectExecution'
  | 'undoDeleteAsset'
  | 'updateAsset'
  | 'updateDirectory'
  | 'updateFile'
  | 'updateOrganization'
  | 'updateProject'
  | 'updateProjectExecution'
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

const INVALIDATION_MAP: Partial<Record<MutationMethod, readonly backendQuery.BackendMethods[]>> = {
  updateUser: ['usersMe'],
  uploadUserPicture: ['usersMe'],
  updateOrganization: ['getOrganization'],
  uploadOrganizationPicture: ['getOrganization'],
  createUserGroup: ['listUserGroups'],
  deleteUserGroup: ['listUserGroups'],
  changeUserGroup: ['listUsers'],
  createTag: ['listTags'],
  deleteTag: ['listTags'],
  createProjectExecution: ['listProjectExecutions'],
  updateProjectExecution: ['listProjectExecutions'],
  deleteProjectExecution: ['listProjectExecutions'],
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
        ...(INVALIDATION_MAP[method]?.map((queryMethod) => [backend?.type, queryMethod]) ?? []),
      ],
      awaitInvalidates: options?.meta?.awaitInvalidates ?? true,
    },
  }
}

// ===================================
// === useBackendMutationVariables ===
// ===================================

/** Access mutation variables from a React Query Mutation. */
export function useBackendMutationVariables<Method extends backendQuery.BackendMethods>(
  backend: Backend | null,
  method: Method,
  mutationKey?: readonly unknown[],
) {
  return reactQuery.useMutationState<Parameters<Backend[Method]>>({
    filters: {
      mutationKey: [backend?.type, method, ...(mutationKey ?? [])],
      status: 'pending',
    },
    // eslint-disable-next-line no-restricted-syntax
    select: (mutation) => mutation.state.variables as never,
  })
}

// ===================
// === Placeholder ===
// ===================

/** An object with a `isPlaceholder` property. */
interface Placeholder {
  readonly isPlaceholder: boolean
}

// =======================
// === WithPlaceholder ===
// =======================

/** An existing type, with an added `isPlaceholder` property. */
export type WithPlaceholder<T extends object> = Placeholder & T

// ========================
// === toNonPlaceholder ===
// ========================

/** Return an object with an additional field `isPlaceholder: false`. */
function toNonPlaceholder<T extends object>(object: T) {
  return { ...object, isPlaceholder: false }
}

// ====================
// === useListUsers ===
// ====================

/** A list of users, taking into account optimistic state. */
export function useListUsers(
  backend: Backend,
): readonly WithPlaceholder<backendModule.User>[] | null {
  const listUsersQuery = useBackendQuery(backend, 'listUsers', [])
  const changeUserGroupVariables = useBackendMutationVariables(backend, 'changeUserGroup')
  return React.useMemo(() => {
    if (listUsersQuery.data == null) {
      return null
    } else {
      const result = listUsersQuery.data.map(toNonPlaceholder)
      const userIdToIndex = new Map(result.map((user, i) => [user.userId, i]))
      for (const [userId, body] of changeUserGroupVariables) {
        const index = userIdToIndex.get(userId)
        const user = index == null ? null : result[index]
        if (index != null && user != null) {
          result[index] = { ...user, userGroups: body.userGroups }
        }
      }
      return result
    }
  }, [changeUserGroupVariables, listUsersQuery.data])
}

// =========================
// === useListUserGroups ===
// =========================

/** A list of user groups, taking into account optimistic state. */
export function useListUserGroups(
  backend: Backend,
): readonly WithPlaceholder<backendModule.UserGroupInfo>[] | null {
  const { user } = authProvider.useFullUserSession()
  const listUserGroupsQuery = useBackendQuery(backend, 'listUserGroups', [])
  const createUserGroupVariables = useBackendMutationVariables(backend, 'createUserGroup')
  const deleteUserGroupVariables = useBackendMutationVariables(backend, 'deleteUserGroup')
  return React.useMemo(() => {
    if (listUserGroupsQuery.data == null) {
      return null
    } else {
      const deletedUserGroupIds = new Set(deleteUserGroupVariables.map(([id]) => id))
      const userGroupsBase = listUserGroupsQuery.data
        .filter((userGroup) => !deletedUserGroupIds.has(userGroup.id))
        .map(toNonPlaceholder)
      return [
        ...createUserGroupVariables.map(([body]) => ({
          organizationId: user.organizationId,
          id: backendModule.newPlaceholderUserGroupId(),
          groupName: body.name,
          isPlaceholder: true,
        })),
        ...userGroupsBase,
      ]
    }
  }, [
    user.organizationId,
    createUserGroupVariables,
    deleteUserGroupVariables,
    listUserGroupsQuery.data,
  ])
}

// ==================================
// === useListUserGroupsWithUsers ===
// ==================================

/** A user group, as well as the users that are a part of the user group. */
export interface UserGroupInfoWithUsers extends backendModule.UserGroupInfo {
  readonly users: readonly WithPlaceholder<backendModule.User>[]
}

/** A list of user groups, taking into account optimistic state. */
export function useListUserGroupsWithUsers(
  backend: Backend,
): readonly WithPlaceholder<UserGroupInfoWithUsers>[] | null {
  const userGroupsRaw = useListUserGroups(backend)
  // Old user list
  const listUsersQuery = useBackendQuery(backend, 'listUsers', [])
  // Current user list, including optimistic updates
  const users = useListUsers(backend)
  return React.useMemo(() => {
    if (userGroupsRaw == null || listUsersQuery.data == null || users == null) {
      return null
    } else {
      const currentUserGroupsById = new Map(
        listUsersQuery.data.map((user) => [user.userId, new Set(user.userGroups)]),
      )
      const result = userGroupsRaw.map((userGroup) => {
        const usersInGroup: readonly WithPlaceholder<backendModule.User>[] = users
          .filter((user) => user.userGroups?.includes(userGroup.id))
          .map((user) => {
            if (currentUserGroupsById.get(user.userId)?.has(userGroup.id) !== true) {
              return { ...user, isPlaceholder: true }
            } else {
              return user
            }
          })
        return { ...userGroup, users: usersInGroup }
      })
      return result
    }
  }, [listUsersQuery.data, userGroupsRaw, users])
}

// ===================
// === useListTags ===
// ===================

/** A list of asset tags, taking into account optimistic state. */
export function useListTags(
  backend: Backend | null,
): readonly WithPlaceholder<backendModule.Label>[] | null {
  const listTagsQuery = useBackendQuery(backend, 'listTags', [])
  const createTagVariables = useBackendMutationVariables(backend, 'createTag')
  const deleteTagVariables = useBackendMutationVariables(backend, 'deleteTag')
  return React.useMemo(() => {
    if (listTagsQuery.data == null) {
      return null
    } else {
      const deletedTags = new Set(deleteTagVariables.map((variables) => variables[0]))
      const result = listTagsQuery.data
        .filter((tag) => !deletedTags.has(tag.id))
        .map(toNonPlaceholder)
      return [
        ...result,
        ...createTagVariables.map((variables) => ({
          id: backendModule.TagId(`tag-${uniqueString.uniqueString()}`),
          value: backendModule.LabelName(variables[0].value),
          color: variables[0].color,
          isPlaceholder: true,
        })),
      ]
    }
  }, [createTagVariables, deleteTagVariables, listTagsQuery.data])
}

// ==================
// === useUsersMe ===
// ==================

/** The current user, taking into account optimistic state. */
export function useUsersMe(backend: Backend | null) {
  const usersMeQuery = useBackendQuery(backend, 'usersMe', [])
  const updateUserVariables = useBackendMutationVariables(backend, 'updateUser')
  const uploadUserPictureVariables = useBackendMutationVariables(backend, 'uploadUserPicture')
  return React.useMemo(() => {
    if (usersMeQuery.data == null) {
      return null
    } else {
      let result = usersMeQuery.data
      for (const [{ username }] of updateUserVariables) {
        if (username != null) {
          result = { ...result, name: username }
        }
      }
      for (const [, file] of uploadUserPictureVariables) {
        result = {
          ...result,
          profilePicture: backendModule.HttpsUrl(createUserPictureUrl(backend?.type, file)),
        }
      }
      return result
    }
  }, [backend, usersMeQuery.data, updateUserVariables, uploadUserPictureVariables])
}

// ==========================
// === useGetOrganization ===
// ==========================

/** The current user's organization, taking into account optimistic state. */
export function useGetOrganization(backend: Backend | null) {
  const getOrganizationQuery = useBackendQuery(backend, 'getOrganization', [])
  const updateOrganizationVariables = useBackendMutationVariables(backend, 'updateOrganization')
  const uploadOrganizationPictureVariables = useBackendMutationVariables(
    backend,
    'uploadOrganizationPicture',
  )
  return React.useMemo(() => {
    if (getOrganizationQuery.data == null) {
      return null
    } else {
      let result = getOrganizationQuery.data
      for (const [update] of updateOrganizationVariables) {
        result = { ...result, ...update }
      }
      for (const [, file] of uploadOrganizationPictureVariables) {
        result = {
          ...result,
          picture: backendModule.HttpsUrl(createOrganizationPictureUrl(backend?.type, file)),
        }
      }
      return result
    }
  }, [
    backend,
    getOrganizationQuery.data,
    updateOrganizationVariables,
    uploadOrganizationPictureVariables,
  ])
}
