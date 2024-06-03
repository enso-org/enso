/** @file Hooks for interacting with the backend. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'
import invariant from 'tiny-invariant'

import * as authProvider from '#/providers/AuthProvider'

import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'

import * as dateTime from '#/utilities/dateTime'
import * as permissions from '#/utilities/permissions'
import * as uniqueString from '#/utilities/uniqueString'

// FIXME: Listeners and optimistic state for duplicateProjectMutation

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
  const { user } = authProvider.useNonPartialUserSession()
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
      filters: { mutationKey: [backend, method], status: 'success' },
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
    >([backend, method], data => (data == null ? data : updater(data)))
  }
  const setQueryDataWithKey = <Method extends keyof Backend>(
    method: Method,
    key: reactQuery.QueryKey,
    updater: (
      variable: Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>
    ) => Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>
  ) => {
    queryClient.setQueryData<
      Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>
    >([backend, method, ...key], data => (data == null ? data : updater(data)))
  }

  // === Users ===

  useObserveMutations('updateUser', state => {
    if (state.variables != null) {
      const [body] = state.variables
      setQueryData('usersMe', currentUser =>
        currentUser == null ? null : { ...currentUser, name: body.username ?? currentUser.name }
      )
    }
  })
  useObserveMutations('uploadUserPicture', state => {
    if (state.data != null) {
      revokeUserPictureUrl(backend)
      const data = state.data
      setQueryData('usersMe', () => data)
    }
  })

  // === Organizations ===

  useObserveMutations('updateOrganization', state => {
    if (state.data != null) {
      const data = state.data
      setQueryData('getOrganization', () => data)
    }
  })
  useObserveMutations('uploadOrganizationPicture', state => {
    if (state.data != null) {
      revokeOrganizationPictureUrl(backend)
      const data = state.data
      setQueryData('getOrganization', () => data)
    }
  })

  // === User groups ===

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
        users.map(otherUser =>
          otherUser.userId !== userId ? otherUser : { ...otherUser, userGroups: body.userGroups }
        )
      )
    }
  })

  // === Tags ===

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

  // === Create assets ===

  const createAssetObject = <T extends Partial<backendModule.AnyAsset>>(rest: T) => ({
    description: null,
    labels: [],
    permissions: permissions.tryGetSingletonOwnerPermission(user),
    projectState: null,
    modifiedAt: dateTime.toRfc3339(new Date()),
    ...rest,
  })

  useObserveMutations('createDirectory', state => {
    if (state.variables != null && state.data != null) {
      const [body] = state.variables
      const data = state.data
      setQueryDataWithKey(
        'listDirectory',
        [data.parentId, backendModule.FilterBy.active],
        items => [
          ...items,
          createAssetObject({
            type: backendModule.AssetType.directory,
            id: data.id,
            title: body.title,
            parentId: data.parentId,
          }),
        ]
      )
    }
  })
  useObserveMutations('createProject', state => {
    if (state.variables != null && state.data != null) {
      const [body] = state.variables
      const data = state.data
      const parentId = body.parentDirectoryId ?? backend?.rootDirectoryId(user)
      if (parentId != null) {
        setQueryDataWithKey('listDirectory', [parentId, backendModule.FilterBy.active], items => [
          ...items,
          createAssetObject({
            type: backendModule.AssetType.project,
            id: data.projectId,
            title: data.name,
            parentId,
            projectState: data.state,
          }),
        ])
      }
    }
  })
  useObserveMutations('createDatalink', state => {
    if (state.variables != null && state.data != null) {
      const [body] = state.variables
      const data = state.data
      const parentId = body.parentDirectoryId ?? backend?.rootDirectoryId(user)
      if (parentId != null) {
        setQueryDataWithKey('listDirectory', [parentId, backendModule.FilterBy.active], items => [
          ...items,
          createAssetObject({
            type: backendModule.AssetType.datalink,
            id: data.id,
            title: body.name,
            parentId,
          }),
        ])
      }
    }
  })
  useObserveMutations('createSecret', state => {
    if (state.variables != null && state.data != null) {
      const [body] = state.variables
      const id = state.data
      const parentId = body.parentDirectoryId ?? backend?.rootDirectoryId(user)
      if (parentId != null) {
        setQueryDataWithKey('listDirectory', [parentId, backendModule.FilterBy.active], items => [
          ...items,
          createAssetObject({
            type: backendModule.AssetType.secret,
            id,
            title: body.name,
            parentId,
          }),
        ])
      }
    }
  })

  // === Update assets ===

  useObserveMutations('uploadFile', state => {
    if (state.data != null && state.variables != null) {
      const [body] = state.variables
      const data = state.data
      const parentId = body.parentDirectoryId ?? backend?.rootDirectoryId(user)
      if (parentId != null) {
        setQueryDataWithKey('listDirectory', [parentId, backendModule.FilterBy.active], items => [
          ...items,
          data.project == null
            ? createAssetObject({
                type: backendModule.AssetType.file,
                id: data.id,
                title: body.fileName,
                parentId,
              })
            : createAssetObject({
                type: backendModule.AssetType.project,
                id: data.project.projectId,
                title: data.project.name,
                projectState: data.project.state,
                parentId,
              }),
        ])
      }
    }
  })
  useObserveMutations('updateAsset', state => {
    if (state.data != null && state.variables != null) {
      const [body] = state.variables
      setQueryDataWithKey('listDirectory', [parentId], items => items)
    }
  })

  // === Delete assets ===

  useObserveMutations('deleteAsset', state => {
    // TODO: update both "active" and "deleted" queries
  })
  useObserveMutations('undoDeleteAsset', state => {
    // TODO: update both "active" and "deleted" queries
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
    queryKey: [backend, method, ...args, ...(options?.queryKey ?? [])],
    // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-return
    queryFn: () => (backend?.[method] as any)?.(...args),
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
    mutationKey: [backend, method, ...(options?.mutationKey ?? [])],
    // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-return
    mutationFn: args => (backend[method] as any)(...args),
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
      mutationKey: [backend, method, ...(mutationKey ?? [])],
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
  invariant(user != null, 'User must exist for user groups to be listed.')
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

/** The directory with the given ID, taking into account optimistic state. */
export function useBackendListDirectory(
  backend: Backend,
  directoryId: backendModule.DirectoryId,
  title: string,
  filterBy = backendModule.FilterBy.active
) {
  const { user } = authProvider.useNonPartialUserSession()
  const listDirectoryQuery = useBackendQuery(
    backend,
    'listDirectory',
    [
      {
        filterBy,
        labels: null,
        parentId: directoryId,
        recentProjects: false,
      },
      title,
    ],
    {
      queryKey: [directoryId, filterBy],
    }
  )
  // FIXME: Create a query storing all the results in a map keyed by parent directory
  // to avoid filtering the entire list for every single component listing the directory.
  // Also create a query for each individual asset.
  const createDirectoryVariables = useBackendMutationVariables(backend, 'createDirectory')
  const createProjectVariables = useBackendMutationVariables(backend, 'createProject')
  const createDatalinkVariables = useBackendMutationVariables(backend, 'createDatalink')
  const createSecretVariables = useBackendMutationVariables(backend, 'createSecret')
  const uploadFileVariables = useBackendMutationVariables(backend, 'uploadFile')
  return React.useMemo(() => {
    if (listDirectoryQuery.data == null) {
      return null
    } else {
      const result = listDirectoryQuery.data.map(toNonPlaceholder)
      const placeholderProjectState = { type: backendModule.ProjectState.new, volumeId: '' }
      const createAssetObject = <T extends Partial<backendModule.AnyAsset>>(rest: T) => ({
        description: null,
        labels: [],
        permissions: permissions.tryGetSingletonOwnerPermission(user),
        projectState: null,
        modifiedAt: dateTime.toRfc3339(new Date()),
        parentId: directoryId,
        isPlaceholder: true,
        ...rest,
      })
      for (const [body] of createDirectoryVariables) {
        result.push(
          createAssetObject({
            type: backendModule.AssetType.directory,
            id: backendModule.DirectoryId(
              `${backendModule.AssetType.directory}-${uniqueString.uniqueString()}`
            ),
            title: body.title,
          })
        )
      }
      for (const [body] of createProjectVariables) {
        result.push(
          createAssetObject({
            type: backendModule.AssetType.project,
            id: backendModule.ProjectId(
              `${backendModule.AssetType.project}-${uniqueString.uniqueString()}`
            ),
            title: body.projectName,
            projectState: placeholderProjectState,
          })
        )
      }
      for (const [body] of createDatalinkVariables) {
        if (body.datalinkId == null) {
          result.push(
            createAssetObject({
              type: backendModule.AssetType.datalink,
              id: backendModule.DatalinkId(
                `${backendModule.AssetType.datalink}-${uniqueString.uniqueString()}`
              ),
              title: body.name,
            })
          )
        }
      }
      for (const [body] of createSecretVariables) {
        result.push(
          createAssetObject({
            type: backendModule.AssetType.secret,
            id: backendModule.SecretId(
              `${backendModule.AssetType.secret}-${uniqueString.uniqueString()}`
            ),
            title: body.name,
          })
        )
      }
      for (const [body] of uploadFileVariables) {
        const projectNameAndExtension = backendModule.extractProjectExtension(body.fileName)
        const projectName =
          projectNameAndExtension.extension === '' ? null : projectNameAndExtension.basename
        result.push(
          projectName == null
            ? createAssetObject({
                type: backendModule.AssetType.file,
                id: backendModule.FileId(
                  `${backendModule.AssetType.file}-${uniqueString.uniqueString()}`
                ),
                title: body.fileName,
              })
            : createAssetObject({
                type: backendModule.AssetType.project,
                id: backendModule.ProjectId(
                  `${backendModule.AssetType.project}-${uniqueString.uniqueString()}`
                ),
                title: projectName,
                projectState: placeholderProjectState,
              })
        )
      }
      return result
    }
  }, [
    directoryId,
    user,
    listDirectoryQuery.data,
    createDatalinkVariables,
    createDirectoryVariables,
    createProjectVariables,
    createSecretVariables,
    uploadFileVariables,
  ])
}
