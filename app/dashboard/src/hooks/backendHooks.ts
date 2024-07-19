/** @file Hooks for interacting with the backend. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import * as backendQuery from 'enso-common/src/backendQuery'

import * as authProvider from '#/providers/AuthProvider'
import { useRemoteBackendStrict } from '#/providers/BackendProvider'

import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'
import type RemoteBackend from '#/services/RemoteBackend'

import * as uniqueString from '#/utilities/uniqueString'

// ==============================
// === BackendMutationOptions ===
// ==============================

/** Mutation options for a specific backend method. */
interface BackendMutationOptions<Method extends MutationMethodInternal>
  extends Omit<
    reactQuery.UseMutationOptions<
      Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>,
      Error,
      Parameters<Extract<Backend[Method], (...args: never) => unknown>>,
      unknown
    >,
    'mutationFn'
  > {}

// ============================
// === revokeUserPictureUrl ===
// ============================

const USER_PICTURE_URL_REVOKERS = new WeakMap<Backend, () => void>()

/** Create the corresponding "user picture" URL for the given backend. */
function createUserPictureUrl(backend: Backend | null, picture: Blob) {
  if (backend != null) {
    USER_PICTURE_URL_REVOKERS.get(backend)?.()
    const url = URL.createObjectURL(picture)
    USER_PICTURE_URL_REVOKERS.set(backend, () => {
      URL.revokeObjectURL(url)
    })
    return url
  } else {
    // This should never happen, so use an arbitrary URL.
    return location.href
  }
}

/** Revoke the corresponding "user picture" URL for the given backend. */
function revokeUserPictureUrl(backend: Backend | null) {
  if (backend != null) {
    USER_PICTURE_URL_REVOKERS.get(backend)?.()
  }
}

// ====================================
// === revokeOrganizationPictureUrl ===
// ====================================

const ORGANIZATION_PICTURE_URL_REVOKERS = new WeakMap<Backend, () => void>()

/** Create the corresponding "organization picture" URL for the given backend. */
function createOrganizationPictureUrl(backend: Backend | null, picture: Blob) {
  if (backend != null) {
    ORGANIZATION_PICTURE_URL_REVOKERS.get(backend)?.()
    const url = URL.createObjectURL(picture)
    ORGANIZATION_PICTURE_URL_REVOKERS.set(backend, () => {
      URL.revokeObjectURL(url)
    })
    return url
  } else {
    // This should never happen, so use an arbitrary URL.
    return location.href
  }
}

/** Revoke the corresponding "organization picture" URL for the given backend. */
function revokeOrganizationPictureUrl(backend: Backend | null) {
  if (backend != null) {
    ORGANIZATION_PICTURE_URL_REVOKERS.get(backend)?.()
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
export type MutationMethodInternal = DefineBackendMethods<
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

/** Names of methods corresponding to mutations.
 *
 * For omitted methods use `use*Mutation` instead. */
export type MutationMethod = Exclude<
  MutationMethodInternal,
  DefineBackendMethods<
    | 'changeUserGroup'
    | 'createTag'
    | 'createUserGroup'
    | 'deleteTag'
    | 'deleteUserGroup'
    | 'updateOrganization'
    | 'updateUser'
    | 'uploadOrganizationPicture'
    | 'uploadUserPicture'
  >
>

// =======================
// === useSetQueryData ===
// =======================

/** A type-safe function to set query data for backend. */
function useBackendSetQueryData(backend: Backend | null) {
  const queryClient = reactQuery.useQueryClient()
  return <Method extends backendQuery.BackendMethods>(
    method: Method,
    updater: (
      variable: Awaited<ReturnType<Backend[Method]>>
    ) => Awaited<ReturnType<Backend[Method]>>
  ) => {
    queryClient.setQueryData<Awaited<ReturnType<Backend[Method]>>>([backend?.type, method], data =>
      data == null ? data : updater(data)
    )
  }
}

// =======================
// === useBackendQuery ===
// =======================

export function useBackendQuery<Method extends backendQuery.BackendMethods>(
  backend: Backend,
  method: Method,
  args: Parameters<Backend[Method]>,
  options?: Omit<
    reactQuery.UseQueryOptions<
      Awaited<ReturnType<Backend[Method]>>,
      Error,
      Awaited<ReturnType<Backend[Method]>>,
      readonly unknown[]
    >,
    'queryFn'
  >
): reactQuery.UseQueryResult<Awaited<ReturnType<Backend[Method]>>>
export function useBackendQuery<Method extends backendQuery.BackendMethods>(
  backend: Backend | null,
  method: Method,
  args: Parameters<Backend[Method]>,
  options?: Omit<
    reactQuery.UseQueryOptions<
      Awaited<ReturnType<Backend[Method]>>,
      Error,
      Awaited<ReturnType<Backend[Method]>>,
      readonly unknown[]
    >,
    'queryFn'
  >
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
    reactQuery.UseQueryOptions<
      Awaited<ReturnType<Backend[Method]>>,
      Error,
      Awaited<ReturnType<Backend[Method]>>,
      readonly unknown[]
    >,
    'queryFn'
  >
) {
  return reactQuery.useQuery<
    Awaited<ReturnType<Backend[Method]>>,
    Error,
    Awaited<ReturnType<Backend[Method]>>,
    readonly unknown[]
  >({
    ...options,
    ...backendQuery.backendQueryOptions(backend, method, args, options?.queryKey),
    // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-return
    queryFn: () => (backend?.[method] as any)?.(...args),
  })
}

// ==========================
// === useBackendMutation ===
// ==========================

/** Wrap a backend method call in a React Query Mutation. */
// eslint-disable-next-line no-restricted-syntax
export const useBackendMutationOptions: <Method extends MutationMethod>(
  backend: Backend,
  method: Method,
  options?: Omit<
    reactQuery.UseMutationOptions<
      Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>,
      Error,
      Parameters<Extract<Backend[Method], (...args: never) => unknown>>,
      unknown
    >,
    'mutationFn'
  >
) => ReturnType<typeof useBackendMutationOptionsInternal<Method>> =
  useBackendMutationOptionsInternal

function useBackendMutationOptionsInternal<Method extends MutationMethodInternal>(
  backend: Backend,
  method: Method,
  options?: Omit<
    reactQuery.UseMutationOptions<
      Awaited<ReturnType<Backend[Method]>>,
      Error,
      Parameters<Backend[Method]>
    >,
    'mutationFn'
  >
): reactQuery.UseMutationOptions<
  Awaited<ReturnType<Backend[Method]>>,
  Error,
  Parameters<Backend[Method]>
>
function useBackendMutationOptionsInternal<Method extends MutationMethodInternal>(
  backend: Backend | null,
  method: Method,
  options?: Omit<
    reactQuery.UseMutationOptions<
      Awaited<ReturnType<Backend[Method]>>,
      Error,
      Parameters<Backend[Method]>
    >,
    'mutationFn'
  >
): reactQuery.UseMutationOptions<
  Awaited<ReturnType<Backend[Method]>> | undefined,
  Error,
  Parameters<Backend[Method]>
>
// This duplicate signature is required:
// The first signature is required, because the second signaure is wider than the first and would
// otherwise match all calls, and return a result that is too wide.
// This signature is required because the last signature is the one that is picked up by generic
// resolution - specifically the `typeof useBackendMutationInternal` above.
function useBackendMutationOptionsInternal<Method extends MutationMethodInternal>(
  backend: Backend,
  method: Method,
  options?: Omit<
    reactQuery.UseMutationOptions<
      Awaited<ReturnType<Backend[Method]>>,
      Error,
      Parameters<Backend[Method]>
    >,
    'mutationFn'
  >
): reactQuery.UseMutationOptions<
  Awaited<ReturnType<Backend[Method]>>,
  Error,
  Parameters<Backend[Method]>
>
/** Wrap a backend method call in a React Query Mutation. */
function useBackendMutationOptionsInternal<Method extends MutationMethodInternal>(
  backend: Backend | null,
  method: Method,
  options?: Omit<
    reactQuery.UseMutationOptions<
      Awaited<ReturnType<Backend[Method]>>,
      Error,
      Parameters<Backend[Method]>
    >,
    'mutationFn'
  >
): reactQuery.UseMutationOptions<
  Awaited<ReturnType<Backend[Method]>>,
  Error,
  Parameters<Backend[Method]>
> {
  return {
    ...options,
    mutationKey: [backend?.type, method, ...(options?.mutationKey ?? [])],
    // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-return
    mutationFn: args => (backend?.[method] as any)?.(...args),
    networkMode: backend?.type === backendModule.BackendType.local ? 'always' : 'online',
  }
}

// ===================================
// === useBackendMutationVariables ===
// ===================================

/** Access mutation variables from a React Query Mutation. */
export function useBackendMutationVariables<Method extends backendQuery.BackendMethods>(
  backend: Backend | null,
  method: Method,
  mutationKey?: readonly unknown[]
) {
  return reactQuery.useMutationState<Parameters<Backend[Method]>>({
    filters: {
      mutationKey: [backend?.type, method, ...(mutationKey ?? [])],
      status: 'pending',
    },
    // eslint-disable-next-line no-restricted-syntax
    select: mutation => mutation.state.variables as never,
  })
}

// ===================================
// === mergeBackendMutationOptions ===
// ===================================

/** Merge mutation options. */
function mergeBackendMutationOptions<Method extends MutationMethodInternal>(
  options: BackendMutationOptions<Method> | undefined,
  toMerge: Required<Pick<BackendMutationOptions<Method>, 'onSuccess'>>
): BackendMutationOptions<Method> {
  return {
    ...options,
    onSuccess: (...args) => {
      options?.onSuccess?.(...args)
      toMerge.onSuccess(...args)
    },
  }
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
  backend: Backend
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
  backend: Backend
): readonly WithPlaceholder<backendModule.UserGroupInfo>[] | null {
  const { user } = authProvider.useNonPartialUserSession()
  const listUserGroupsQuery = useBackendQuery(backend, 'listUserGroups', [])
  const createUserGroupVariables = useBackendMutationVariables(backend, 'createUserGroup')
  const deleteUserGroupVariables = useBackendMutationVariables(backend, 'deleteUserGroup')
  return React.useMemo(() => {
    if (listUserGroupsQuery.data == null) {
      return null
    } else {
      const deletedUserGroupIds = new Set(deleteUserGroupVariables.map(([id]) => id))
      const userGroupsBase = listUserGroupsQuery.data
        .filter(userGroup => !deletedUserGroupIds.has(userGroup.id))
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
  backend: Backend
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
        listUsersQuery.data.map(user => [user.userId, new Set(user.userGroups)])
      )
      const result = userGroupsRaw.map(userGroup => {
        const usersInGroup: readonly WithPlaceholder<backendModule.User>[] = users
          .filter(user => user.userGroups?.includes(userGroup.id))
          .map(user => {
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
  backend: Backend | null
): readonly WithPlaceholder<backendModule.Label>[] | null {
  const listTagsQuery = useBackendQuery(backend, 'listTags', [])
  const createTagVariables = useBackendMutationVariables(backend, 'createTag')
  const deleteTagVariables = useBackendMutationVariables(backend, 'deleteTag')
  return React.useMemo(() => {
    if (listTagsQuery.data == null) {
      return null
    } else {
      const deletedTags = new Set(deleteTagVariables.map(variables => variables[0]))
      const result = listTagsQuery.data
        .filter(tag => !deletedTags.has(tag.id))
        .map(toNonPlaceholder)
      return [
        ...result,
        ...createTagVariables.map(variables => ({
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
          profilePicture: backendModule.HttpsUrl(createUserPictureUrl(backend, file)),
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
    'uploadOrganizationPicture'
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
          picture: backendModule.HttpsUrl(createOrganizationPictureUrl(backend, file)),
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

// =============================
// === useUpdateUserMutation ===
// =============================

/** A mutation to update a user's information. */
export function useUpdateUserMutation(options?: BackendMutationOptions<'updateUser'>) {
  const backend = useRemoteBackendStrict()
  const { authQueryKey } = authProvider.useAuth()
  return useBackendMutationOptionsInternal(backend, 'updateUser', {
    ...options,
    meta: {
      invalidates: [...(options?.meta?.invalidates ?? []), authQueryKey],
      awaitInvalidates: options?.meta?.awaitInvalidates ?? true,
    },
  })
}

// ==========================================
// === createRemoteBackendMutationBuilder ===
// ==========================================

/** Return a function to build mutation options for a specific backend method. */
function createRemoteBackendMutationBuilder<
  Method extends MutationMethodInternal,
  ExtraHooks extends object = object,
>(
  method: Method,
  onSuccess: (
    context: ExtraHooks & {
      backend: RemoteBackend
      setQueryData: ReturnType<typeof useBackendSetQueryData>
    },
    ...args: Parameters<NonNullable<BackendMutationOptions<Method>['onSuccess']>>
  ) => void,
  useExtraHooks?: () => ExtraHooks
) {
  /** A function to build mutation options for a specific backend method. */
  // eslint-disable-next-line no-restricted-syntax
  function useBuilder(options?: BackendMutationOptions<Method>) {
    const backend = useRemoteBackendStrict()
    const setQueryData = useBackendSetQueryData(backend)
    // eslint-disable-next-line no-restricted-syntax
    const extraHooks = useExtraHooks?.() as ExtraHooks
    return useBackendMutationOptionsInternal(
      backend,
      method,
      mergeBackendMutationOptions(options, {
        onSuccess: (...args) => {
          onSuccess({ backend, setQueryData, ...extraHooks }, ...args)
        },
      })
    )
  }
  return useBuilder
}

// ====================================
// === useUploadUserPictureMutation ===
// ====================================

/** A mutation to upload a user's profile picture. */
export const useUploadUserPictureMutation = createRemoteBackendMutationBuilder(
  'uploadUserPicture',
  ({ backend, setQueryData, setUser }, data) => {
    revokeUserPictureUrl(backend)
    setQueryData('usersMe', () => data)
    setUser(data)
  },
  () => {
    const { setUser } = authProvider.useAuth()
    return { setUser }
  }
)

// =====================================
// === useUpdateOrganizationMutation ===
// =====================================

/** A mutation to update an organization's information. */
export const useUpdateOrganizationMutation = createRemoteBackendMutationBuilder(
  'updateOrganization',
  ({ setQueryData }, data) => {
    setQueryData('getOrganization', () => data)
  }
)

// ============================================
// === useUploadOrganizationPictureMutation ===
// ============================================

/** A mutation to upload an organization's profile picture. */
export const useUploadOrganizationPictureMutation = createRemoteBackendMutationBuilder(
  'uploadOrganizationPicture',
  ({ backend, setQueryData }, data) => {
    revokeOrganizationPictureUrl(backend)
    setQueryData('getOrganization', () => data)
  }
)

// ==================================
// === useCreateUserGroupMutation ===
// ==================================

/** A mutation to create a user group. */
export const useCreateUserGroupMutation = createRemoteBackendMutationBuilder(
  'createUserGroup',
  ({ setQueryData }, data) => {
    setQueryData('listUserGroups', userGroups => [data, ...userGroups])
  }
)

// ==================================
// === useDeleteUserGroupMutation ===
// ==================================

/** A mutation to delete a user group. */
export const useDeleteUserGroupMutation = createRemoteBackendMutationBuilder(
  'deleteUserGroup',
  ({ setQueryData }, _data, variables) => {
    setQueryData('listUserGroups', userGroups =>
      userGroups.filter(userGroup => userGroup.id !== variables[0])
    )
  }
)

// ==================================
// === useChangeUserGroupMutation ===
// ==================================

/** A mutation to change a user group. */
export const useChangeUserGroupMutation = createRemoteBackendMutationBuilder(
  'changeUserGroup',
  ({ setQueryData }, _data, [userId, body]) => {
    setQueryData('listUsers', users =>
      users.map(user => (user.userId !== userId ? user : { ...user, userGroups: body.userGroups }))
    )
  }
)

// ============================
// === useCreateTagMutation ===
// ============================

/** A mutation to create a tag. */
export const useCreateTagMutation = createRemoteBackendMutationBuilder(
  'createTag',
  ({ setQueryData }, data) => {
    setQueryData('listTags', tags => [...tags, data])
  }
)

// ============================
// === useDeleteTagMutation ===
// ============================

/** A mutation to delete a tag. */
export const useDeleteTagMutation = createRemoteBackendMutationBuilder(
  'deleteTag',
  ({ setQueryData }, _data, [tagId]) => {
    setQueryData('listTags', tags => tags.filter(tag => tag.id !== tagId))
  }
)
