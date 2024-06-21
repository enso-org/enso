/** @file Hooks for interacting with the backend. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import * as authProvider from '#/providers/AuthProvider'

import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'

import * as uniqueString from '#/utilities/uniqueString'

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

// =========================
// === useObserveBackend ===
// =========================

/** Listen to all mutations and update state as appropriate when they succeed.
 * MUST be unconditionally called exactly once for each backend type. */
export function useObserveBackend(backend: Backend | null) {
  const queryClient = reactQuery.useQueryClient()
  const [seen] = React.useState(new WeakSet())
  const useObserveMutations = <Method extends keyof Backend>(
    method: Method,
    onSuccess: (
      state: reactQuery.MutationState<
        Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>,
        Error,
        Parameters<Extract<Backend[Method], (...args: never) => unknown>>
      >
    ) => void
  ) => {
    const states = reactQuery.useMutationState<
      Parameters<Extract<Backend[Method], (...args: never) => unknown>>
    >({
      // Errored mutations can be safely ignored as they should not change the state.
      filters: { mutationKey: [backend?.type, method], status: 'success' },
      // eslint-disable-next-line no-restricted-syntax
      select: mutation => mutation.state as never,
    })
    for (const state of states) {
      if (!seen.has(state)) {
        seen.add(state)
        // This is SAFE - it is just too highly dynamic for TypeScript to typecheck.
        // eslint-disable-next-line no-restricted-syntax
        onSuccess(state as never)
      }
    }
  }
  const setQueryData = <Method extends keyof Backend>(
    method: Method,
    updater: (
      variable: Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>
    ) => Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>
  ) => {
    queryClient.setQueryData<
      Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>
    >([backend?.type, method], data => (data == null ? data : updater(data)))
  }
  useObserveMutations('uploadUserPicture', state => {
    revokeUserPictureUrl(backend)
    setQueryData('usersMe', user => state.data ?? user)
  })
  useObserveMutations('updateOrganization', state => {
    setQueryData('getOrganization', organization => state.data ?? organization)
  })
  useObserveMutations('uploadOrganizationPicture', state => {
    revokeOrganizationPictureUrl(backend)
    setQueryData('getOrganization', organization => state.data ?? organization)
  })
  useObserveMutations('createUserGroup', state => {
    if (state.data != null) {
      const data = state.data
      setQueryData('listUserGroups', userGroups => [data, ...userGroups])
    }
  })
  useObserveMutations('deleteUserGroup', state => {
    setQueryData('listUserGroups', userGroups =>
      userGroups.filter(userGroup => userGroup.id !== state.variables?.[0])
    )
  })
  useObserveMutations('changeUserGroup', state => {
    if (state.variables != null) {
      const [userId, body] = state.variables
      setQueryData('listUsers', users =>
        users.map(user =>
          user.userId !== userId ? user : { ...user, userGroups: body.userGroups }
        )
      )
    }
  })
  useObserveMutations('createTag', state => {
    if (state.data != null) {
      const data = state.data
      setQueryData('listTags', tags => [...tags, data])
    }
  })
  useObserveMutations('deleteTag', state => {
    if (state.variables != null) {
      const [tagId] = state.variables
      setQueryData('listTags', tags => tags.filter(tag => tag.id !== tagId))
    }
  })
}

// =======================
// === useBackendQuery ===
// =======================

export function useBackendQuery<Method extends keyof Backend>(
  backend: Backend,
  method: Method,
  args: Parameters<Extract<Backend[Method], (...args: never) => unknown>>,
  options?: Omit<
    reactQuery.UseQueryOptions<
      Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>,
      Error,
      Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>,
      readonly unknown[]
    >,
    'queryFn'
  >
): reactQuery.UseQueryResult<
  Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>
>
export function useBackendQuery<Method extends keyof Backend>(
  backend: Backend | null,
  method: Method,
  args: Parameters<Extract<Backend[Method], (...args: never) => unknown>>,
  options?: Omit<
    reactQuery.UseQueryOptions<
      Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>,
      Error,
      Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>,
      readonly unknown[]
    >,
    'queryFn'
  >
): reactQuery.UseQueryResult<
  // eslint-disable-next-line no-restricted-syntax
  Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>> | undefined
>
/** Wrap a backend method call in a React Query. */
export function useBackendQuery<Method extends keyof Backend>(
  backend: Backend | null,
  method: Method,
  args: Parameters<Extract<Backend[Method], (...args: never) => unknown>>,
  options?: Omit<
    reactQuery.UseQueryOptions<
      Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>,
      Error,
      Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>,
      readonly unknown[]
    >,
    'queryFn'
  >
) {
  return reactQuery.useQuery<
    Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>,
    Error,
    Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>,
    readonly unknown[]
  >({
    ...options,
    queryKey: [backend?.type, method, ...args, ...(options?.queryKey ?? [])],
    // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-return
    queryFn: () => (backend?.[method] as any)?.(...args),
    networkMode: backend?.type === backendModule.BackendType.local ? 'always' : 'online',
  })
}

// ==========================
// === useBackendMutation ===
// ==========================

/** Wrap a backend method call in a React Query Mutation. */
export function useBackendMutation<Method extends keyof Backend>(
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
) {
  return reactQuery.useMutation<
    Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>,
    Error,
    Parameters<Extract<Backend[Method], (...args: never) => unknown>>,
    unknown
  >({
    ...options,
    mutationKey: [backend.type, method, ...(options?.mutationKey ?? [])],
    // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-return
    mutationFn: args => (backend[method] as any)(...args),
    networkMode: backend.type === backendModule.BackendType.local ? 'always' : 'online',
  })
}

// ===================================
// === useBackendMutationVariables ===
// ===================================

/** Access mutation variables from a React Query Mutation. */
export function useBackendMutationVariables<Method extends keyof Backend>(
  backend: Backend | null,
  method: Method,
  mutationKey?: readonly unknown[]
) {
  return reactQuery.useMutationState<
    Parameters<Extract<Backend[Method], (...args: never) => unknown>>
  >({
    filters: {
      mutationKey: [backend?.type, method, ...(mutationKey ?? [])],
      status: 'pending',
    },
    // eslint-disable-next-line no-restricted-syntax
    select: mutation => mutation.state.variables as never,
  })
}

// =======================================
// === useBackendMutationWithVariables ===
// =======================================

/** Wrap a backend method call in a React Query Mutation, and access its variables. */
export function useBackendMutationWithVariables<Method extends keyof Backend>(
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
) {
  const mutation = useBackendMutation(backend, method, options)
  return {
    mutation,
    mutate: mutation.mutate,
    mutateAsync: mutation.mutateAsync,
    variables: useBackendMutationVariables(backend, method, options?.mutationKey),
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

// ===========================
// === useBackendListUsers ===
// ===========================

/** A list of users, taking into account optimistic state. */
export function useBackendListUsers(
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

// ================================
// === useBackendListUserGroups ===
// ================================

/** A list of user groups, taking into account optimistic state. */
export function useBackendListUserGroups(
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

// =========================================
// === useBackendListUserGroupsWithUsers ===
// =========================================

/** A user group, as well as the users that are a part of the user group. */
export interface UserGroupInfoWithUsers extends backendModule.UserGroupInfo {
  readonly users: readonly WithPlaceholder<backendModule.User>[]
}

/** A list of user groups, taking into account optimistic state. */
export function useBackendListUserGroupsWithUsers(
  backend: Backend
): readonly WithPlaceholder<UserGroupInfoWithUsers>[] | null {
  const userGroupsRaw = useBackendListUserGroups(backend)
  // Old user list
  const listUsersQuery = useBackendQuery(backend, 'listUsers', [])
  // Current user list, including optimistic updates
  const users = useBackendListUsers(backend)
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

// ==========================
// === useBackendListTags ===
// ==========================

/** A list of asset tags, taking into account optimistic state. */
export function useBackendListTags(
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

// =========================
// === useBackendUsersMe ===
// =========================

/** The current user, taking into account optimistic state. */
export function useBackendUsersMe(backend: Backend | null) {
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

// =================================
// === useBackendGetOrganization ===
// =================================

/** The current user's organization, taking into account optimistic state. */
export function useBackendGetOrganization(backend: Backend | null) {
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
